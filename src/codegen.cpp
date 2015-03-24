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
	PType *type = search_for_type(*env, value.type);
	return type ? *type : PType();
}

PType CodeGenerator::lookup_type(char *value) {
	PType *type = search_for_type(*env, value);
	return type ? *type : PType();
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
	result.type = "bool";
	result.value = ConstantInt::get(getGlobalContext(),
	                                APInt(lookup_type("bool").numbits, value));
	return result;
}

AllocaInst *CodeGenerator::create_entry_block_alloca(char *name, Type *type) {
	PFunction function = lookup_function(env->current_function);
	if (!function.return_type || !function.value) {
		fatal_error("invalid 'current' function for alloca creation\n");
		return NULL;
	}

	BasicBlock *entry_bb = &function.value->getEntryBlock();
	IRBuilder<> entry_builder(entry_bb, entry_bb->begin());
	return entry_builder.CreateAlloca(type, 0, name);
}

bool CodeGenerator::implicit_type_convert(PValue *source, char *dest_typename) {
	PType source_type = lookup_type(source->type);
	PType dest_type = lookup_type(dest_typename);
	if (source_type == dest_type)
		return true;

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
			if (CastInst::isCastable(source_type.type, dest_type.type)) {
				source->type = dest_typename;
				auto cast_opcode = CastInst::getCastOpcode(source->value,
				                                           source_type.is_signed,
				                                           dest_type.type,
				                                           dest_type.is_signed);
				source->value = builder->CreateCast(cast_opcode,
				                                    source->value,
				                                    dest_type.type);
				return true;
			}
		}
	}

	return false;
}

bool CodeGenerator::explicit_type_convert(PValue *source, char *dest_typename) {
	PType source_type = lookup_type(source->type);
	PType dest_type = lookup_type(dest_typename);
	if (source_type == dest_type)
		return true;

	// For now, we'll just do any casts that LLVM thinks we can do.
	// Think about this more in future (incl. bitcasts, etc.).
	if (CastInst::isCastable(source_type.type, dest_type.type)) {
		source->type = dest_typename;
		auto cast_opcode = CastInst::getCastOpcode(source->value,
		                                           source_type.is_signed,
		                                           dest_type.type,
		                                           dest_type.is_signed);
		source->value = builder->CreateCast(cast_opcode,
		                                    source->value,
		                                    dest_type.type);
		return true;
	}

	return false;
}

PVariable CodeGenerator::generate_variable_declaration(ASTNode *node) {
	PVariable result;

	switch (node->type) {
		case NODE_VARIABLE_DECLARATION:
		{
			DECL_ASTNODE_DATA(node, variable_declaration, pnode);
			char *variable_name = pnode.name->data.string.value;
			char *variable_type_name = NULL;
			if (pnode.type)
				variable_type_name = pnode.type->data.string.value;
			assert(variable_type_name || pnode.init);
			if (env->symbol_table.exists(variable_name)) {
				set_error(pnode.name, "variable naming conflict");
				break;
			}

			PValue value;
			if (pnode.init) {
				value = generate_expression(pnode.init);
				if (error) {
					break;
				} else if (variable_type_name) {
					assert(implicit_type_convert(&value, variable_type_name));
				} else { // Type inference
					// TODO: Do we really want small int literals to type infer to uint8?
					// That seems kinda dumb, as I suspect people will expect (u?)int32.
					result.type = value.type;
				}
			}
			if (variable_type_name)
				result.type = variable_type_name;

			// alloca at the entry block so that the mem2reg pass can hit
			PType variable_type = lookup_type(result.type);
			result.value = create_entry_block_alloca(variable_name,
			                                         variable_type.type);
			env->symbol_table.set(variable_name, result);

			if (pnode.init)
				builder->CreateStore(value.value, result.value);
			break;
		}
		default:
			set_error(node, "failed to generate variable for ASTNode");
			break;
	}

	return result;
}

PValue CodeGenerator::generate_expression(ASTNode *node) {
	PValue result;

	switch (node->type) {
		case NODE_BINARY_OPERATOR:
		{
			DECL_ASTNODE_DATA(node, binary_operator, pnode);

			// TODO: Handle other operators (+=, etc.)
			if (!strcmp(pnode.value, "=")) {
				DECL_ASTNODE_DATA(pnode.left, string, symbol_name);
				PVariable var_sym = lookup_symbol(symbol_name.value);
				if (!var_sym.type) {
					set_error(pnode.left, "invalid symbol name for assignment");
					break;
				}
				PValue value = generate_expression(pnode.right);
				if (error)
					break;

				if (!implicit_type_convert(&value, var_sym.type)) {
					set_error(pnode.right, "type mismatch in assignment - expected '%s', "
					          "got '%s'", var_sym.type, value.type);
					break;
				}

				builder->CreateStore(value.value, var_sym.value);
				result.type = value.type;
				result.value = value.value;
				break;
			}


			PValue left = generate_expression(pnode.left);
			if (error)
				break;
			PValue right = generate_expression(pnode.right);
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
				          "'%s', got '%s'", left.type, right.type);
				break;
			}

			PType type = lookup_type(left);
			if (!type.is_numeric) {
				set_error(pnode.right,
				          "non-numeric type '%s' specified for binary operation",
				          left.type);
				break;
			}

			// TODO: Handle other operators (>=, etc.)
			// NOTE: We use 'ordered' floating point comparisons below. I think this
			// is the correct decision here, but I'm not entirely sure. Check!
			result.type = left.type;
			if (!strcmp(pnode.value, "==")) {
				result.type = "bool";
				if (type.is_float)
					result.value = builder->CreateFCmpOEQ(left.value,
					                                      right.value, "eqtmp");
				else
					result.value = builder->CreateICmpEQ(left.value,
					                                     right.value, "eqtmp");
			} else if (!strcmp(pnode.value, "!=")) {
				result.type = "bool";
				if (type.is_float)
					result.value = builder->CreateFCmpONE(left.value,
					                                      right.value, "neqtmp");
				else
					result.value = builder->CreateICmpNE(left.value,
					                                     right.value, "neqtmp");
			} else if (!strcmp(pnode.value, "+")) {
				if (type.is_float)
					result.value = builder->CreateFAdd(left.value, right.value, "addtmp");
				else
					result.value = builder->CreateAdd(left.value, right.value, "addtmp");
			} else if (!strcmp(pnode.value, "*")) {
				if (type.is_float)
					result.value = builder->CreateFMul(left.value, right.value, "multmp");
				else
					result.value = builder->CreateMul(left.value, right.value, "multmp");
			} else if (!strcmp(pnode.value, "/")) {
				if (type.is_float)
					result.value = builder->CreateFDiv(left.value, right.value, "divtmp");
				else if (type.is_signed)
					result.value = builder->CreateSDiv(left.value, right.value, "divtmp");
				else
					result.value = builder->CreateUDiv(left.value, right.value, "divtmp");
			} else if (!strcmp(pnode.value, "-")) {
				if (type.is_float)
					result.value = builder->CreateFSub(left.value, right.value, "subtmp");
				else
					result.value = builder->CreateSub(left.value, right.value, "subtmp");
			}
			break;
		}
		// case NODE_UNARY_OPERATOR:
		// 	// TODO: What if we have an unsigned type negated by a unary operator?
		// 	// I guess we perform an implicit type conversion?
		//
		// 	break;
		case NODE_CAST_OPERATOR:
		{
			DECL_ASTNODE_DATA(node, unary_operator, pnode);
			PValue value = generate_expression(pnode.operand);
			if (error)
				break;
			if (!explicit_type_convert(&value, pnode.value)) {
				set_error(node, "failed to convert type '%s' to '%s'",
				          value.type, pnode.value);
				break;
			}

			result.type = value.type;
			result.value = value.value;
			break;
		}
		case NODE_FUNCTION_CALL:
		{
			DECL_ASTNODE_DATA(node, function_call, pnode);
			char *function_name = pnode.name->data.string.value;
			Function *function = module->getFunction(function_name);
			PFunction pfunction = lookup_function(function_name);
			if (!function || !pfunction.return_type) {
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
				PValue arg = generate_expression(pnode.args[i]);
				if (error)
					break;
				if (!implicit_type_convert(&arg, pfunction.arg_types[i])) {
					set_error(node, "function parameter mis-match at param %d", i);
					break;
				}
				assert(!strcmp(arg.type, pfunction.arg_types[i]));
				args.add(arg.value);
			}
			if (error)
				break;

			result.type = pfunction.return_type;
			result.value = builder->CreateCall(function,
			                                   ArrayRef<Value *>(args.getPointer(0),
			                                                     args_count),
			                                   "calltmp");
			break;
		}
		case NODE_CONSTANT_INT:
		{
			// TODO: Need to deal with hex, etc.
			DECL_ASTNODE_DATA(node, string, pnode);
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

			// Integer literals are unsigned by default, for now at least.
			// Values are packaged into the smallest type in which they fit, as they
			// can be implicitly casted up to larger types when necessary.
			int numbits = 0;
			if (value <= 0xff) {
				result.type = "uint8";
				numbits = 8;
			} else if (value <= 0xffff) {
				result.type = "uint16";
				numbits = 16;
			} else if (value <= 0xffffffff) {
				result.type = "uint32";
				numbits = 32;
			} else if (value <= 0xffffffffffffffff) {
				result.type = "uint64";
				numbits = 64;
			} else {
				set_error(node, "integer literal too large");
				break;
			}

			result.value = ConstantInt::get(getGlobalContext(),
			                                APInt(numbits, value, false));
			break;
		}
		case NODE_CONSTANT_BOOL:
		{
			DECL_ASTNODE_DATA(node, integer, pnode);
			result = get_boolean_value(pnode.value);
			result.type = "bool";
			break;
		}
		case NODE_CONSTANT_FLOAT:
		{
			// TODO: This seems like a pretty terrible way to initialize an APFloat.
			// TODO: Also, we probably want to deal with oversized floats or whatever
			// here.
			DECL_ASTNODE_DATA(node, string, pnode);
			result.type = "float64";
			APFloat number(0.0);
			number.convertFromString(pnode.value, APFloat::rmNearestTiesToEven);
			result.value = ConstantFP::get(getGlobalContext(), number);
			break;
		}
		case NODE_IDENTIFIER:
		{
			DECL_ASTNODE_DATA(node, string, pnode);
			PVariable value = lookup_symbol(pnode.value);
			if (!value.type) {
				set_error(node, "failed to find symbol '%s'", pnode.value);
				break;
			}

			result.type = value.type;
			result.value = builder->CreateLoad(value.value, pnode.value);
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
			DECL_ASTNODE_DATA(node, function_signature, pnode);
			Environment *previous_env = env;
			env = pnode.env;

			size_t args_count = pnode.args.size();
			MemoryList<Type *> args(args_count);
			for (size_t i = 0; i < args_count; ++i) {
				DECL_ASTNODE_DATA(pnode.args[i], variable_declaration, arg);

				char *type_name = arg.type->data.string.value;
				result.arg_types.add(type_name);

				PType type = lookup_type(type_name);
				args.add(type.type);
			}
			if (error)
				break;

			// TODO: Needle to handle redefinitions
			char *function_name = pnode.name->data.string.value;
			char *function_type_name = pnode.type->data.string.value;
			FunctionType *function_type;
			Function *function;
			function_type = FunctionType::get(lookup_type(function_type_name).type,
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
				if (!pfunction.return_type) {
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
				result.value = function;
				result.return_type = pnode.type->data.string.value;
				previous_env->function_table.set(function_name, result);
			}

			env = previous_env;
			break;
		}
		case NODE_FUNCTION:
		{
			DECL_ASTNODE_DATA(node, function, pnode);
			PFunction function = generate_function(pnode.signature);
			if (error)
				break;

			BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry",
			                                    function.value);
			builder->SetInsertPoint(BB);

			Environment *prev_env = env; // TODO: Helper for env push/pop
			env = pnode.signature->data.function_signature.env;

			{
				DECL_ASTNODE_DATA(pnode.signature, function_signature, psignature);
				MemoryList<ASTNode *> args = psignature.args;
				Function *funcval = function.value;
				Function::arg_iterator it;
				size_t i;
				for (it = funcval->arg_begin(), i = 0; i < args.size(); ++i, ++it) {
					generate_variable_declaration(args[i]);
					if (error)
						break;

					DECL_ASTNODE_DATA(args[i], variable_declaration, arg);
					char *param_name = arg.name->data.string.value;
					it->setName(param_name);
					PVariable param = lookup_symbol(param_name);
					builder->CreateStore(it, param.value);
				}
			}

			generate_statement(pnode.body);
			if (error)
				break;

			env = prev_env;

			result = function;
			verifyFunction(*function.value);
			fpm->run(*function.value);
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
			DECL_ASTNODE_DATA(node, statements, pnode);
			for (size_t i = 0; i < pnode.children.size(); ++i) {
				generate_statement(pnode.children[i]);
				if (error)
					break;
			}
			break;
		}
		case NODE_BLOCK:
		{
			DECL_ASTNODE_DATA(node, block, pnode);
			Environment *prev_env = env;
			env = pnode.env;
			generate_statement(pnode.statements);
			env = prev_env;
			break;
		}
		case NODE_UNARY_OPERATOR:
		case NODE_BINARY_OPERATOR:
			generate_expression(node);
			break;
		case NODE_FUNCTION_SIGNATURE:
		case NODE_FUNCTION:
			generate_function(node);
			break;
		case NODE_FUNCTION_CALL:
			generate_expression(node);
			break;
		case NODE_VARIABLE_DECLARATION:
			generate_variable_declaration(node);
			break;
		case NODE_IF:
		{
			DECL_ASTNODE_DATA(node, conditional, pnode);
			PValue cond = generate_expression(pnode.condition);
			if (error)
				break;
			if (lookup_type(cond) != lookup_type("bool")) {
				set_error(pnode.condition,
				          "unsupported type for if statement condition: '%s'",
				          cond.type);
				break;
			}

			PValue true_val = get_boolean_value(true);
			cond.value = builder->CreateICmpEQ(cond.value, true_val.value, "ifcond");

			Function *function;
			BasicBlock *then, *otherwise, *merge;

			function = builder->GetInsertBlock()->getParent();
			then = BasicBlock::Create(getGlobalContext(), "then", function);
			otherwise = BasicBlock::Create(getGlobalContext(), "else", function);
			merge = BasicBlock::Create(getGlobalContext(), "ifcont", function);
			builder->CreateCondBr(cond.value, then, otherwise);

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
			DECL_ASTNODE_DATA(node, conditional, pnode);
			Function *function;
			BasicBlock *preloop, *loop, *after;

			function = builder->GetInsertBlock()->getParent();
			preloop = BasicBlock::Create(getGlobalContext(), "prewhile", function);
			loop = BasicBlock::Create(getGlobalContext(), "while", function);
			after = BasicBlock::Create(getGlobalContext(), "afterwhile", function);
			builder->CreateBr(preloop);

			builder->SetInsertPoint(preloop);
			PValue cond = generate_expression(pnode.condition);
			if (error)
				break;

			if (lookup_type(cond) != lookup_type("bool")) {
				set_error(pnode.condition,
				          "unsupported type for while statement condition: %s",
				          cond.type);
				break;
			}

			cond.value = builder->CreateICmpEQ(cond.value,
			                                   get_boolean_value(true).value,
			                                   "whilecond");
			builder->CreateCondBr(cond.value, loop, after);

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
			DECL_ASTNODE_DATA(node, unary_operator, pnode);
			PValue val;
			if (pnode.operand) {
				val = generate_expression(pnode.operand);
				if (error)
					break;
			} else {
				val.value = NULL;
			}

			PFunction function = lookup_function(env->current_function);
			if (!implicit_type_convert(&val, function.return_type)) {
				set_error(pnode.operand,
				          "type mismatch in return statement - expected '%s', got '%s'",
				          function.return_type, val.type);
				break;
			}

			builder->CreateRet(val.value);
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
