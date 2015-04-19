#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/CallingConv.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Transforms/Scalar.h"

#include "codegen.h"
#include "hashmap.hpp"
#include "environment.h"
#include "helpers.h"

using namespace llvm;

// TODO: The quality of errors in here needs improving.

// TODO: At some point, we need to focus on quality of the LLVM IR generated.
//       [COMPARE TO: $ clang -S -emit-llvm foo.c]

void CodeGenerator::set_error(ASTNode *node, const char *format, ...) {
	free(error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	error = buffer;
	errnode = node;
}

PType CodeGenerator::lookup_type(PValue value) {
	return lookup_type(value.type);
}

PType CodeGenerator::lookup_type(PExType extype) {
	PType result;
	PType *type = search_for_type(*env, const_cast<char *>(extype.type_name));
	if (type) {
		result = *type;
		for (unsigned int i = 0; i < extype.pointer_level; ++i)
			result.llvmty = PointerType::get(result.llvmty, 0);

		// Other modifiers can go here
	}

	return result;
}

PFunction CodeGenerator::lookup_function(char *name) {
	PFunction *function = search_for_function(*env, name);
	return function ? *function : PFunction();
}

PVariable CodeGenerator::lookup_symbol(char *name) {
	PVariable *symbol = search_for_symbol(*env, name);
	return symbol ? *symbol : PVariable();
}

PValue CodeGenerator::get_boolean_value(bool value) {
	PValue result;
	result.type = PExType("bool");
	result.llvmval = ConstantInt::get(getGlobalContext(),
	                                  APInt(lookup_type(result.type).numbits,
	                                  value));
	return result;
}

AllocaInst *CodeGenerator::create_entry_block_alloca(char *name, Type *type) {
	PFunction function = lookup_function(env->current_function);
	if (!function.return_type.is_set() || !function.llvmval) {
		fatal_error("invalid 'current' function for alloca creation\n");
		return NULL;
	}

	BasicBlock *entry_bb = &function.llvmval->getEntryBlock();
	IRBuilder<> entry_builder(entry_bb, entry_bb->begin());
	return entry_builder.CreateAlloca(type, 0, name);
}

bool CodeGenerator::implicit_type_convert(PValue *source,
                                          PExType dest_extype)
{
	PType source_type = lookup_type(source->type);
	PType dest_type = lookup_type(dest_extype);
	if (source_type == dest_type)
		return true;

	// Pointers can only cast to themselves at the moment, so this check works
	// fine. Will likely need to change this as other modifiers get introduced.
	if (source->type.pointer_level != dest_extype.pointer_level)
		return false;

	if (source_type.is_numeric && dest_type.is_numeric) {
		// TODO: Consider whether integer to float implicit conversion is a good
		// idea or not.
		// TODO: This big unwieldy condition can probably be simplified down!
		if ((((source_type.is_signed == dest_type.is_signed
		 && (!source_type.is_float))
		 || (source_type.is_float && dest_type.is_float))
		 && ((source_type.numbits <= dest_type.numbits)
		 || (!source_type.is_float && dest_type.is_float)))

		 // NOTE: unsigned->signed conversions are allowed if there is no possible
		 // truncation. implicit signed->unsigned conversions are disallowed.
		 // TODO: Should we be more relaxed with this check? I'm unsure.
		 || ((!source_type.is_signed && dest_type.is_signed)
		 && (dest_type.numbits - 1 >= source_type.numbits))) {
			// We could easily create the cast instructions manually rather than
			// relying on 'getCastOpcode' here if required (Trunc, ZExt/SExt, etc.).
			if (CastInst::isCastable(source_type.llvmty, dest_type.llvmty)) {
				source->type = dest_extype;
				auto cast_opcode = CastInst::getCastOpcode(source->llvmval,
				                                           source_type.is_signed,
				                                           dest_type.llvmty,
				                                           dest_type.is_signed);
				source->llvmval = builder->CreateCast(cast_opcode,
				                                      source->llvmval,
				                                      dest_type.llvmty);
				return true;
			}
		}
	}

	return false;
}

bool CodeGenerator::explicit_type_convert(PValue *source,
                                          PExType dest_extype)
{
	PType source_type = lookup_type(source->type);
	PType dest_type = lookup_type(dest_extype);
	if (source_type == dest_type)
		return true;

	// For now, we'll just do any casts that LLVM thinks we can do.
	// Think about this more in future (incl. bitcasts, etc.).
	if (CastInst::isCastable(source_type.llvmty, dest_type.llvmty)) {
		source->type = dest_extype;
		auto cast_opcode = CastInst::getCastOpcode(source->llvmval,
		                                           source_type.is_signed,
		                                           dest_type.llvmty,
		                                           dest_type.is_signed);
		source->llvmval = builder->CreateCast(cast_opcode,
		                                      source->llvmval,
		                                      dest_type.llvmty);
		return true;
	}

	return false;
}

// NOTE: This generates mis-matching types and values - the caller should beware
// that the type returned is for the loaded value, not the value returned (as,
// in the context of this member function, the loaded value is the thing of
// importance).
PVariable CodeGenerator::generate_lvalue(ASTNode *node) {
	PVariable result;

	switch (node->type) {
		case NODE_VARIABLE_DECLARATION:
		{
			auto pnode = *node->toVariableDeclaration();
			char *variable_name = pnode.name->toString()->value;
			PExType variable_extype;
			if (pnode.type.is_set())
				variable_extype = pnode.type;
			assert(variable_extype.is_set() || pnode.init);
			if (env->symbol_table.exists(variable_name)) {
				set_error(pnode.name, "variable naming conflict");
				break;
			}

			PValue value;
			if (pnode.init) {
				value = generate_rvalue(pnode.init);
				if (error) {
					break;
				} else if (variable_extype.is_set()) {
					if (!implicit_type_convert(&value, variable_extype)) {
						set_error(pnode.init, "type mismatch in variable initialization - "
						          "expected '%s', got '%s'", variable_extype.to_string(),
					            value.type.to_string());
						break;
					}
				} else { // Type inference
					// TODO: Maybe we want to do something with the type here.
					// If we're using an 8-bit int literal, for example, we might want
					// to actually declare the variable as 32-bits in size.
					result.type = value.type;
				}
			}
			if (variable_extype.is_set())
				result.type = variable_extype;

			// alloca at the entry block so that the mem2reg pass can hit
			PType variable_type = lookup_type(result.type);
			result.llvmval = create_entry_block_alloca(variable_name,
			                                           variable_type.llvmty);
			env->symbol_table.set(variable_name, result);

			if (pnode.init)
				builder->CreateStore(value.llvmval, result.llvmval);
			break;
		}
		case NODE_IDENTIFIER:
		{
			auto pnode = *node->toString();
			PVariable value = lookup_symbol(pnode.value);
			if (!value.type.is_set()) {
				set_error(node, "failed to find symbol '%s'", pnode.value);
				break;
			}

			result.type = value.type;
			result.llvmval = value.llvmval;
			break;
		}
		case NODE_UNARY_OPERATOR:
		{
			auto pnode = *node->toUnaryOperator();

			if (!strcmp(pnode.value, "*")) {
				PValue value = generate_rvalue(pnode.operand);

				if (!value.type.pointer_level) {
					set_error(pnode.operand, "cannot dereference non-pointer operand");
					break;
				}
				result.type = value.type;
				result.type.pointer_level -= 1;
				result.llvmval = (AllocaInst *)value.llvmval;
				break;
			}

			set_error(node, "unsupported unary operator '%s'", pnode.value);
			break;
		}
		default:
			set_error(node, "failed to generate variable for ASTNode");
			break;
	}

	return result;
}

PValue CodeGenerator::generate_rvalue(ASTNode *node) {
	PValue result;

	switch (node->type) {
		case NODE_BINARY_OPERATOR:
		{
			auto pnode = *node->toBinaryOperator();

			// TODO: Handle other operators (+=, etc.)
			if (!strcmp(pnode.value, "=")) {
				// TODO: God fucking damn it. We need the lvalue here (AllocaInst *) not
				// the 'Value *' from the load.
				PVariable left = generate_lvalue(pnode.left);
				if (!isa<AllocaInst>(left.llvmval)) {
					set_error(pnode.left, "value on left of assignment is not an lvalue");
					break;
				}

				PValue value = generate_rvalue(pnode.right);
				if (error)
					break;

				if (!implicit_type_convert(&value, left.type)) {
					set_error(pnode.right, "type mismatch in assignment - expected '%s', "
					          "got '%s'", left.type.to_string(),
					          value.type.to_string());
					break;
				}

				builder->CreateStore(value.llvmval, (AllocaInst *)left.llvmval);
				result.type = value.type;
				result.llvmval = value.llvmval;
				break;
			}


			PValue left = generate_rvalue(pnode.left);
			if (error)
				break;
			PValue right = generate_rvalue(pnode.right);
			if (error)
				break;

			// TODO: Ensure these binop cast rules are sensible at some point.
			PType left_type = lookup_type(left);
			PType right_type = lookup_type(right);
			bool cast_left = (!left_type.is_signed && right_type.is_signed)
			              || (left_type.numbits < right_type.numbits)
			              || (right_type.is_float);
			bool cast_success = false;
			if (cast_left)
				cast_success = implicit_type_convert(&left, right.type);
			else
				cast_success = implicit_type_convert(&right, left.type);

			if (!cast_success) {
				set_error(pnode.right, "type mismatch in binary operation - expected "
				          "'%s', got '%s'", left.type.to_string(),
				          right.type.to_string());
				break;
			}

			PExType extype = left.type;
			PType type = lookup_type(extype);
			if (!type.is_numeric) {
				set_error(pnode.right,
				          "non-numeric type '%s' specified for binary operation",
				          left.type.to_string());
				break;
			}

			// TODO: Handle pointer (+ other modifiers?) in operations!
			// NOTE: This may be harder than I expected as it appears that it's the
			// 'getelementptr' instruction that actually gets given the offset, not
			// the pointers themselves. Additionally, 'pointer - pointer' shouldn't be
			// too hard in theory, but I'm not sure what type it should return.
			// This is all non-trivial, take a little while to think about it!
			if (extype.pointer_level) {
				set_error(node, "pointer types are not yet supported in binary "
				          "expressions.");
				break;
			}
			
			// TODO: Handle other operators (>=, etc.)
			// NOTE: We use 'ordered' floating point comparisons below. I think this
			// is the correct decision here, but I'm not entirely sure. Check!
			result.type = left.type;
			if (!strcmp(pnode.value, "==")) {
				result.type = PExType("bool");
				if (type.is_float)
					result.llvmval = builder->CreateFCmpOEQ(left.llvmval,
					                                        right.llvmval, "eqtmp");
				else
					result.llvmval = builder->CreateICmpEQ(left.llvmval,
					                                       right.llvmval, "eqtmp");
			} else if (!strcmp(pnode.value, "!=")) {
				result.type = PExType("bool");
				if (type.is_float)
					result.llvmval = builder->CreateFCmpONE(left.llvmval,
					                                        right.llvmval, "neqtmp");
				else
					result.llvmval = builder->CreateICmpNE(left.llvmval,
					                                       right.llvmval, "neqtmp");
			} else if (!strcmp(pnode.value, "+")) {
				if (type.is_float)
					result.llvmval = builder->CreateFAdd(left.llvmval,
					                                     right.llvmval, "addtmp");
				else
					result.llvmval = builder->CreateAdd(left.llvmval,
					                                    right.llvmval, "addtmp");
			} else if (!strcmp(pnode.value, "*")) {
				if (type.is_float)
					result.llvmval = builder->CreateFMul(left.llvmval,
					                                     right.llvmval, "multmp");
				else
					result.llvmval = builder->CreateMul(left.llvmval,
					                                    right.llvmval, "multmp");
			} else if (!strcmp(pnode.value, "/")) {
				if (type.is_float)
					result.llvmval = builder->CreateFDiv(left.llvmval,
					                                     right.llvmval, "divtmp");
				else if (type.is_signed)
					result.llvmval = builder->CreateSDiv(left.llvmval,
					                                     right.llvmval, "divtmp");
				else
					result.llvmval = builder->CreateUDiv(left.llvmval,
					                                     right.llvmval, "divtmp");
			} else if (!strcmp(pnode.value, "-")) {
				if (type.is_float)
					result.llvmval = builder->CreateFSub(left.llvmval,
					                                     right.llvmval, "subtmp");
				else
					result.llvmval = builder->CreateSub(left.llvmval,
					                                    right.llvmval, "subtmp");
			}
			break;
		}
		case NODE_UNARY_OPERATOR:
		{
			auto pnode = *node->toUnaryOperator();

			// TODO: What if we have an unsigned type negated by a unary operator?
			// (Particularly, if we're negating an int literal)
		
			// TODO: Eventually handle dereference operator here.

			if (!strcmp(pnode.value, "&")) {
				if (pnode.operand->type != NODE_IDENTIFIER) {
					set_error(pnode.operand, "invalid operand to '&'");
					break;
				}

				auto symbol_name = *pnode.operand->toString();
				PVariable variable = lookup_symbol(symbol_name.value);
				if (!variable.type.is_set()) {
					set_error(pnode.operand, "invalid symbol name for '&' operator");
					break;
				}

				result.type = variable.type;
				result.type.pointer_level += 1;
				result.llvmval = variable.llvmval;
				break;
			} else if (!strcmp(pnode.value, "*")) {
				PVariable value = generate_lvalue(node);
				result.type = value.type;
				result.llvmval = builder->CreateLoad(value.llvmval);
				break;
			} else if (!strcmp(pnode.value, "+")) {
				PValue value = generate_rvalue(pnode.operand);
				PType type = lookup_type(value.type);
				if (!type.is_numeric) {
					set_error(node, "'+' may only be used on numeric types");
					break;
				}

				result = value;
				break;
			}

			set_error(node, "unsupported unary operator '%s'", pnode.value);
			break;
		}
		case NODE_CAST_OPERATOR:
		{
			auto pnode = *node->toCastOperator();
			PValue value = generate_rvalue(pnode.operand);
			if (error)
				break;
			if (!explicit_type_convert(&value, pnode.type)) {
				set_error(node, "failed to convert type '%s' to '%s'",
				          value.type.to_string(), pnode.type.to_string());
				break;
			}

			result.type = value.type;
			result.llvmval = value.llvmval;
			break;
		}
		case NODE_FUNCTION_CALL:
		{
			auto pnode = *node->toFunctionCall();
			char *function_name = pnode.name->toString()->value;
			Function *function = module->getFunction(function_name);
			PFunction pfunction = lookup_function(function_name);
			if (!function || !pfunction.return_type.is_set()) {
				set_error(pnode.name, "unknown function referenced '%s'",
				          function_name);
				break;
			}

			size_t args_count = pnode.args.size();
			if (args_count != pfunction.arg_types.size()) {
				set_error(node,
				          "function parameter number mismatch - expected %d, got %d",
				          pfunction.arg_types.size(), args_count);
				break;
			}

			MemoryList<Value *> args(args_count);
			for (size_t i = 0; i < args_count; ++i) {
				PValue arg = generate_rvalue(pnode.args[i]);
				if (error)
					break;
				if (!implicit_type_convert(&arg, pfunction.arg_types[i])) {
					set_error(node, "function parameter mismatch at param %d - "
					          "expected '%s', got '%s'",
					          i + 1, pfunction.arg_types[i].to_string(),
					          arg.type.to_string());
					break;
				}

				assert(arg.type == pfunction.arg_types[i]);
				args.add(arg.llvmval);
			}
			if (error)
				break;

			result.type = pfunction.return_type;
			result.llvmval = builder->CreateCall(function,
			                                     ArrayRef<Value *>(args.getPointer(0),
			                                                       args_count),
			                                     "calltmp");
			break;
		}
		case NODE_CONSTANT_INT:
		{
			// TODO: Need to deal with hex, etc.
			auto pnode = *node->toString();
			char *endptr = pnode.value;
			unsigned long long value = strtoull(pnode.value, &endptr, 10);
			assert(sizeof(value) >= 8);
			if (endptr == pnode.value
			 || (size_t)(endptr - pnode.value) < (strlen(pnode.value))) {
				set_error(node, "failed to parse invalid integer literal");
				break;
			}
			if (errno == ERANGE) {
				set_error(node, "integer literal too large");
				break;
			}

			// Integer literals are unsigned by default, and are put into the smallest
			// type they fit in.
			int numbits = 0;
			if (value <= 0xff) {
				result.type = PExType("uint8");
			} else if (value <= 0xffff) {
				result.type = PExType("uint16");
			} else if (value <= 0xffffffff) {
				result.type = PExType("uint32");
			} else if (value <= 0xffffffffffffffff) {
				result.type = PExType("uint64");
			} else {
				set_error(node, "integer literal too large");
				break;
			}
			numbits = lookup_type(result.type).numbits;

			result.llvmval = ConstantInt::get(getGlobalContext(),
			                                  APInt(numbits, value, false));
			break;
		}
		case NODE_CONSTANT_BOOL:
		{
			auto pnode = *node->toInteger();
			result = get_boolean_value(pnode.value);
			result.type = PExType("bool");
			break;
		}
		case NODE_CONSTANT_FLOAT:
		{
			// TODO: This seems like a pretty terrible way to initialize an APFloat.
			// TODO: Also, we probably want to deal with oversized floats or whatever
			// here.
			auto pnode = *node->toString();
			result.type = PExType("float64");
			APFloat number(0.0);
			number.convertFromString(pnode.value, APFloat::rmNearestTiesToEven);
			result.llvmval = ConstantFP::get(getGlobalContext(), number);
			break;
		}
		case NODE_IDENTIFIER:
		{
			auto pnode = *node->toString();
			PVariable value = generate_lvalue(node);
			result.type = value.type;
			result.llvmval = builder->CreateLoad(value.llvmval, pnode.value);
			break;
		}
		default:
			set_error(node, "failed to generate expression for ASTNode");
			break;
	}

	return result;
}

PFunction CodeGenerator::generate_function(ASTNode *node) {
	PFunction result;

	switch (node->type) {
		case NODE_FUNCTION_SIGNATURE:
		{
			auto pnode = *node->toFunctionSignature();
			Environment *previous_env = env;
			env = pnode.env;

			size_t args_count = pnode.args.size();
			MemoryList<Type *> args(args_count);
			for (size_t i = 0; i < args_count; ++i) {
				auto arg = *pnode.args[i]->toVariableDeclaration();

				PExType extype = arg.type;
				result.arg_types.add(extype);

				PType type = lookup_type(extype);
				args.add(type.llvmty);
			}
			if (error)
				break;

			// TODO: Need to handle redefinitions
			char *function_name = pnode.name->toString()->value;
			PExType extype = pnode.type;
			FunctionType *function_type;
			Function *function;
			function_type = FunctionType::get(lookup_type(extype).llvmty,
			                                  ArrayRef<Type *>(args.getPointer(0),
			                                                   args_count),
			                                  false);
			function = Function::Create(function_type, Function::ExternalLinkage,
			                            function_name, module);
			function->addFnAttr(Attribute::NoUnwind);

			// If the name we got back isn't the one we assigned, there was a conflict
			if (function->getName() != function_name) {
				// Erase the just-created signature, and get the previous one.
				function->eraseFromParent();
				function = module->getFunction(function_name);

				if (!function->empty()) {
					set_error(node, "redefinition of function is not allowed");
					break;
				}

				PFunction pfunction = lookup_function(function_name);
				if (!pfunction.return_type.is_set()) {
					set_error(pnode.name,
					          "conflict between LLVM and function table state");
					break;
				}

				if (pfunction.arg_types.size() != result.arg_types.size()) {
					set_error(node, "redefinition of function with parameter number "
					          "mismatch - expected %d, got %d",
					          pfunction.arg_types.size(), args_count);
					break;
				}
				for (size_t i = 0; i < pfunction.arg_types.size(); ++i) {
					PType expected_type = lookup_type(pfunction.arg_types[i]);
					PType actual_type = lookup_type(result.arg_types[i]);
					if (actual_type != expected_type) {
						set_error(node, "redefinition of function with parameter mis-match "
						          "at param %d", i);
						break;
					}
				}
				if (error)
					break;

				result = pfunction;
			} else {
				result.llvmval = function;
				result.return_type = pnode.type;
				previous_env->function_table.set(function_name, result);
			}

			env = previous_env;
			break;
		}
		case NODE_FUNCTION:
		{
			auto pnode = *node->toFunction();
			PFunction function = generate_function(pnode.signature);
			if (error)
				break;

			BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry",
			                                    function.llvmval);
			builder->SetInsertPoint(BB);

			Environment *prev_env = env; // TODO: Helper for env push/pop
			env = pnode.signature->toFunctionSignature()->env;

			{
				auto psignature = *pnode.signature->toFunctionSignature();
				MemoryList<ASTNode *> args = psignature.args;
				Function *funcval = function.llvmval;
				Function::arg_iterator it;
				size_t i;
				for (it = funcval->arg_begin(), i = 0; i < args.size(); ++i, ++it) {
					generate_lvalue(args[i]);
					if (error)
						break;

					auto arg = *args[i]->toVariableDeclaration();
					char *param_name = arg.name->toString()->value;
					it->setName(param_name);
					PVariable param = lookup_symbol(param_name);
					builder->CreateStore(it, param.llvmval);
				}
			}

			generate_statement(pnode.body);
			if (error)
				break;

			env = prev_env;

			result = function;
			verifyFunction(*function.llvmval);
			fpm->run(*function.llvmval);
			break;
		}
		default:
			set_error(node, "unexpected token for function code generation");
			break;
	}

	return result;
}

void CodeGenerator::generate_statement(ASTNode *node) {
	switch (node->type) {
		case NODE_STATEMENTS:
		{
			auto pnode = *node->toStatements();
			for (size_t i = 0; i < pnode.children.size(); ++i) {
				generate_statement(pnode.children[i]);
				if (error)
					break;
			}
			break;
		}
		case NODE_BLOCK:
		{
			auto pnode = *node->toBlock();
			Environment *prev_env = env;
			env = pnode.env;
			generate_statement(pnode.statements);
			env = prev_env;
			break;
		}
		case NODE_UNARY_OPERATOR:
		case NODE_BINARY_OPERATOR:
			generate_rvalue(node);
			break;
		case NODE_FUNCTION_SIGNATURE:
		case NODE_FUNCTION:
			generate_function(node);
			break;
		case NODE_FUNCTION_CALL:
			generate_rvalue(node);
			break;
		case NODE_VARIABLE_DECLARATION:
			generate_lvalue(node);
			break;
		case NODE_IF:
		{
			auto pnode = *node->toConditional();
			PValue cond = generate_rvalue(pnode.condition);
			if (error)
				break;
			if (lookup_type(cond) != lookup_type("bool")) {
				set_error(pnode.condition,
				          "unsupported type for if statement condition: '%s'",
				          cond.type.to_string());
				break;
			}

			PValue true_val = get_boolean_value(true);
			cond.llvmval = builder->CreateICmpEQ(cond.llvmval,
			                                     true_val.llvmval, "ifcond");

			Function *function;
			BasicBlock *then, *otherwise, *merge;

			function = builder->GetInsertBlock()->getParent();
			then = BasicBlock::Create(getGlobalContext(), "then", function);
			otherwise = BasicBlock::Create(getGlobalContext(), "else", function);
			merge = BasicBlock::Create(getGlobalContext(), "ifcont", function);
			builder->CreateCondBr(cond.llvmval, then, otherwise);

			builder->SetInsertPoint(then);
			generate_statement(pnode.then);
			if (error)
				break;
			builder->CreateBr(merge);

			builder->SetInsertPoint(otherwise);
			if (pnode.otherwise) {
				generate_statement(pnode.otherwise);
				if (error)
					break;
			}
			builder->CreateBr(merge);

			builder->SetInsertPoint(merge);
			break;
		}
		case NODE_WHILE_LOOP:
		{
			auto pnode = *node->toConditional();
			Function *function;
			BasicBlock *preloop, *loop, *after;

			function = builder->GetInsertBlock()->getParent();
			preloop = BasicBlock::Create(getGlobalContext(), "prewhile", function);
			loop = BasicBlock::Create(getGlobalContext(), "while", function);
			after = BasicBlock::Create(getGlobalContext(), "afterwhile", function);
			builder->CreateBr(preloop);

			builder->SetInsertPoint(preloop);
			PValue cond = generate_rvalue(pnode.condition);
			if (error)
				break;

			if (lookup_type(cond) != lookup_type("bool")) {
				set_error(pnode.condition,
				          "unsupported type for while statement condition: %s",
				          cond.type.to_string());
				break;
			}

			cond.llvmval = builder->CreateICmpEQ(cond.llvmval,
			                                     get_boolean_value(true).llvmval,
			                                     "whilecond");
			builder->CreateCondBr(cond.llvmval, loop, after);

			// TODO: Handle 'otherwise' branch (while..else)

			builder->SetInsertPoint(loop);
			generate_statement(pnode.then);
			if (error)
				break;
			builder->CreateBr(preloop);
			builder->SetInsertPoint(after);
			break;
		}
		case NODE_RETURN:
		{
			auto pnode = *node->toUnaryOperator();
			PValue val;
			if (pnode.operand) {
				val = generate_rvalue(pnode.operand);
				if (error)
					break;
			} else {
				val.llvmval = NULL;
			}

			PFunction function = lookup_function(env->current_function);
			if (!implicit_type_convert(&val, function.return_type)) {
				set_error(pnode.operand,
				          "type mismatch in return statement - expected '%s', got '%s'",
				          function.return_type.to_string(), val.type.to_string());
				break;
			}

			builder->CreateRet(val.llvmval);
			break;
		}
		case NODE_DO_LOOP:
		case NODE_UNTIL_LOOP:
		case NODE_FOR_LOOP:
		case NODE_BREAK:
		case NODE_CONTINUE:
			set_error(node, "feature not yet implemented!");
			break;
		case NODE_CONSTANT_INT:
		case NODE_CONSTANT_FLOAT:
		case NODE_CONSTANT_STRING:
			break;
		default:
			set_error(node, "failed to generate statement for ASTNode");
			break;
	}
}

void CodeGenerator::generate() {
	InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
	IRBuilder<> builder = IRBuilder<>(getGlobalContext());
	this->builder = &builder;
	module = new Module("program", getGlobalContext());

	error = NULL;
	errnode = NULL;

	// TODO: Add module metadata here (inc. DataLayout)

	// TODO: Think about alignment at some point. It seems like LLVM gives a lot
	// of support for data alignment, but we're not currently using it.

	// TODO: Think about (and extend) code passes (optimisation, etc.)
	// Additionally, the order of these needs to be properly thought about!
	FunctionPassManager fpm(module);
	fpm.add(createBasicAliasAnalysisPass());
	fpm.add(createPromoteMemoryToRegisterPass());
	fpm.add(createReassociatePass());
	fpm.add(createConstantPropagationPass());
	fpm.add(createDeadCodeEliminationPass());
	fpm.add(createGVNPass());
	fpm.add(createCFGSimplificationPass());
	fpm.add(createInstructionCombiningPass());
	fpm.doInitialization();
	this->fpm = &fpm;

	generate_statement(root);
	if (error)
		return;
	verifyModule(*module);

	module->dump();
	delete module;
}
