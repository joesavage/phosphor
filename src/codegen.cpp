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
	result.value = ConstantInt::get(getGlobalContext(), APInt(value ? 1 : 0, 0));
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

PVariable CodeGenerator::generate_variable_declaration(ASTNode *node) {
	PVariable result;

	switch (node->type) {
		case NODE_VARIABLE_DECLARATION:
		{
			DECL_ASTNODE_DATA(node, variable_declaration, pnode);
			char *variable_name = pnode.name->data.string.value;
			char *variable_type_name = pnode.type->data.string.value;
			PType variable_type = lookup_type(variable_type_name);
			if (env->symbol_table.exists(variable_name)) {
				set_error(pnode.type, "variable naming conflict");
				break;
			}

			// alloca at the entry block so that the mem2reg pass can hit
			result.type = variable_type_name;
			result.value = create_entry_block_alloca(variable_name,
			                                         variable_type.type);
			env->symbol_table.set(variable_name, result);
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

				if (lookup_type(value) != lookup_type(var_sym.type)) {
					// TODO: Type conversions (factor out) - for integers, using Trunc or
					// ZExt/SExt (getIntegerBitWidth).
					set_error(pnode.right, "type mismatch in assignment");
					break;
				}

				builder->CreateStore(value.value, var_sym.value);
				result.value = value.value;
				break;
			}


			PValue left = generate_expression(pnode.left);
			if (error)
				break;
			PValue right = generate_expression(pnode.right);
			if (error)
				break;

			if (lookup_type(left) != lookup_type(right)) {
				set_error(pnode.right, "type mismatch in addition operation!");
				break;
			}

			// Right now, we only have hardcoded support for (32-bit) integers.
			if (lookup_type(left).type != IntegerType::get(getGlobalContext(), 32)) {
				set_error(pnode.right, "unsupported type for binary operation");
				break;
			}

			result.type = left.type;
			if (!strcmp(pnode.value, "==")) {
				result.type = "bool";
				result.value = builder->CreateICmpEQ(left.value, right.value, "eqtmp");
			} else if (!strcmp(pnode.value, "!=")) {
				result.type = "bool";
				result.value = builder->CreateICmpNE(left.value, right.value, "eqtmp");
			} else if (!strcmp(pnode.value, "+")) {
				result.value = builder->CreateAdd(left.value, right.value, "addtmp");
			} else if (!strcmp(pnode.value, "*")) {
				result.value = builder->CreateMul(left.value, right.value, "multmp");
			} else if (!strcmp(pnode.value, "/")) {
				// TODO: Handle unsigned
				result.value = builder->CreateSDiv(left.value, right.value, "divtmp");
			} else if (!strcmp(pnode.value, "-")) {
				result.value = builder->CreateSub(left.value, right.value, "subtmp");
			}
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
				args.add(arg.value);
				if (lookup_type(arg.type) != lookup_type(pfunction.arg_types[i])) {
					set_error(node, "function parameter mis-match at param %d", i);
					break;
				}
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
			// TODO: Types aren't 32-bit by default for any reason right now.
			// Just an arbitrary decision that needs reviewing in future.
			//
			// TODO: If we want an int literal max size instead of just wrapping,
			// we should add that here.
			//
			// TODO: Need to deal with hex, etc. (probably earlier than this stage of
			// compilation).
			DECL_ASTNODE_DATA(node, string, pnode);
			result.type = "int32";
			result.value = ConstantInt::get(getGlobalContext(),
			                                APInt(32, StringRef(pnode.value), 10));
			break;
		}
		case NODE_CONSTANT_BOOL:
		{
			DECL_ASTNODE_DATA(node, integer, pnode);
			result.type = "bool";
			// TODO: Use the type table information to construct (e.g. numbits) rather
			// than duplicating it.
			result.value = ConstantInt::get(getGlobalContext(),
			                                APInt(1, pnode.value));
			break;
		}
		case NODE_CONSTANT_FLOAT:
		{
			// TODO: This seems like a pretty terrible way to initialize an APFloat.
			// TODO: Also, we probably want to deal with oversized floats or whatever
			// here.
			DECL_ASTNODE_DATA(node, string, pnode);
			result.type = "float32";
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

			// Set up the function parameters (their symbol table ptrs and names)
			{
				Function::arg_iterator it;
				size_t i;
				for (it = function->arg_begin(), i = 0; i < args.size(); ++i, ++it) {
					DECL_ASTNODE_DATA(pnode.args[i], variable_declaration, arg);
					char *param_name = arg.name->data.string.value;
					it->setName(param_name);
				}
			}

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
					set_error(node, "redefinition of function with parameter number \
					          mismatch - expected %d, got %d", pfunction.arg_types.size(),
					          args_count);
					break;
				}
				for (size_t i = 0; i < pfunction.arg_types.size(); ++i) {
					PType expected_type = lookup_type(pfunction.arg_types[i]);
					PType actual_type = lookup_type(result.arg_types[i]);
					if (actual_type != expected_type) {
						set_error(node, "redefinition of function with parameter mis-match \
						          at param %d", i);
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

			DECL_ASTNODE_DATA(pnode.signature, function_signature, psignature);
			for (size_t i = 0; i < psignature.args.size(); ++i) {
				generate_variable_declaration(psignature.args[i]);
				if (error)
					break;
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
				          "unsupported type for if statement condition");
				break;
			}

			PValue true_val = get_boolean_value(true);
			cond.value = builder->CreateICmpNE(cond.value, true_val.value, "ifcond");

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
				          "unsupported type for while statement condition");
				break;
			}

			cond.value = builder->CreateICmpNE(cond.value,
			                                   ConstantInt::get(getGlobalContext(),
			                                                    APInt(1, 0)),
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

			// TODO: Type check! How to know the function we currently reside in?
			// I guess the current 'env' should hold this information?
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
	FunctionPassManager fpm(module);
	fpm.add(createBasicAliasAnalysisPass());
	fpm.add(createPromoteMemoryToRegisterPass());
	fpm.add(createInstructionCombiningPass());
	fpm.add(createReassociatePass());
	fpm.add(createConstantPropagationPass());
	fpm.add(createDeadCodeEliminationPass());
	fpm.add(createGVNPass());
	fpm.add(createCFGSimplificationPass());
	fpm.doInitialization();
	this->fpm = &fpm;

	generate_statement(root);
	if (error)
		return;
	verifyModule(*module);

	module->dump();
	delete module;
}
