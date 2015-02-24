#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/CallingConv.h"

#include <vector> // TODO: Remove and replace with dynamic c-style array

#include "codegen.h"
#include "hashmap.hpp"
#include "environment.h"
#include "helpers.h"

using namespace llvm;

// TODO: This needs proper error and type handling just EVERYWHERE right now.
//       (the way errors bubble through the system needs better handling also)

// TODO: At some point, we need to focus on quality of the LLVM IR generated.
//       [COMPARE TO: $ clang -S -emit-llvm foo.c]

// TODO: Move this somewhere nicer?
static void codegen_statement(CodeGenerator *generator, ASTNode *node);

// TODO: Move this into the CodeGenerator struct. Or something.
static IRBuilder<> builder(getGlobalContext());

static void codegen_error(CodeGenerator *generator, const char *format, ...) {
	free(generator->error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	generator->error = buffer;
}

static Value *codegen_expression(CodeGenerator *generator, ASTNode *node) {
	Value *result = NULL;

	switch (node->type) {
		case NODE_BINARY_OPERATOR:
		{
			if (!strcmp(node->string.value, "=")) {
				// TODO: Assignments should truncate to the appropriate type, or cause a type error.
				// [For integers, can just construct an APInt with a lower 'numBits'?]

				Value *val = codegen_expression(generator, node->binary_operator.right);

				// TODO: This side-effect means that all assignment expressions end up being immediately executed.
				// This is obviously less than ideal, and so we need to implement mutable state by allocating memory.
				search_for_symbol(*generator->env, node->binary_operator.left->string.value)->value.value = val;
				break;
			}


			Value *left = codegen_expression(generator, node->binary_operator.left);
			Value *right = codegen_expression(generator, node->binary_operator.right);

			// TODO: Realistically, type checking for default hardcoded operations
			// should be more complicated than this (type promotion, etc.).
			if (left->getType() != right->getType()) {
				codegen_error(generator, "type mismatch in addition operation!");
				break;
			}

			if (left->getType() != IntegerType::get(getGlobalContext(), 32)) {
				// Right now, we only have hardcoded support for (32-bit) integers.
				codegen_error(generator, "unsupported type for binary operation");
				break;
			}

			if (!strcmp(node->string.value, "+")) {
				result = builder.CreateAdd(left, right, "addtmp");
			} else if (!strcmp(node->string.value, "*")) {
				result = builder.CreateMul(left, right, "multmp");
			} else if (!strcmp(node->string.value, "/")) {
				// TODO: Handle unsigned
				result = builder.CreateSDiv(left, right, "divtmp");
			} else if (!strcmp(node->string.value, "-")) {
				result = builder.CreateSub(left, right, "subtmp");
			}
			break;
		}
		case NODE_FUNCTION_CALL:
		{
			char *function_name = node->function_call.name->string.value;
			Function *function = generator->module->getFunction(function_name);
			if (!function) {
				codegen_error(generator, "unknown function referenced '%s'", function_name);
				break;
			}

			std::vector<Value *> args;
			for (size_t i = 0; i < node->function_call.args.size(); ++i)
				args.push_back(codegen_expression(generator, node->function_call.args[i]));

			// TODO: Check arguments match the signature! (no. and type)

			result = builder.CreateCall(function, args, "calltmp");
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
			return ConstantInt::get(getGlobalContext(), APInt(32, StringRef(node->string.value), 10));
		}
		case NODE_CONSTANT_FLOAT:
		{
			// TODO: This is a really terrible way to initialize an APFloat.
			// TODO: Also, we probably want to deal with oversized floats or whatever
			// here.
			APFloat number(0.0);
			number.convertFromString(node->string.value, APFloat::rmNearestTiesToEven);
			return ConstantFP::get(getGlobalContext(), number);
		}
		case NODE_IDENTIFIER:
		{
			// TODO: Error handling, type handling, etc. etc. etc.
			Value *val = search_for_symbol(*generator->env, node->string.value)->value.value;
			return val;
		}
		default:
			codegen_error(generator, "failed to generate expression for ASTNode");
			break;
	}

	return result;
}

static Function *codegen_function(CodeGenerator *generator, ASTNode *node) {
	switch (node->type) {
		case NODE_FUNCTION_SIGNATURE:
		{
			Environment *previous_env = generator->env;
			generator->env = node->function_signature.env;

			std::vector<Type *> args;
			for (size_t i = 0; i < node->function_signature.args.size(); ++i) {
				ASTNode *arg = node->function_signature.args[i];
				codegen_statement(generator, arg);
				args.push_back(search_for_type(*generator->env, arg->variable_declaration.type->string.value)->value);
			}

			// TODO: Needle to handle redefinitions
			char *function_name = node->function_signature.name->string.value;
			FunctionType *function_type = FunctionType::get(search_for_type(*generator->env, node->function_signature.type->string.value)->value, args, false);
			Function *function = Function::Create(function_type, Function::ExternalLinkage, function_name, generator->module);

			// If the name we got back isn't the one we assigned, there was a conflict
			if (function->getName() != function_name) {
				// Erase the just-created signature, and get the previous one.
				function->eraseFromParent();
				function = generator->module->getFunction(function_name);

				if (!function->empty()) {
					codegen_error(generator, "redefinition of function is not allowed");
					return NULL;
				}

				// TODO: What if the arguments of the two differ (in size or types)?
			}

			generator->env = previous_env;
			return function;
		}
		case NODE_FUNCTION:
		{
			Function *function = codegen_function(generator, node->function.signature);
			BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", function);
			builder.SetInsertPoint(BB);

			Environment *prev_env = generator->env; // TODO: Helper for env push/pop
			generator->env = node->function.signature->function_signature.env;

			codegen_statement(generator, node->function.body);

			generator->env = prev_env;

			return function;
		}
		default:
			codegen_error(generator, "unexpected token for function code generation");
			break;
	}

	return NULL;
}

static void codegen_statement(CodeGenerator *generator, ASTNode *node) {
	switch (node->type) {
		case NODE_STATEMENTS:
		{
			Environment *previous_env = generator->env;
			generator->env = node->block.env;
			do {
				if (node->block.left)
					codegen_statement(generator, node->block.left);
				if (generator->error)
					return;
			} while ((node = node->block.right));
			generator->env = previous_env;
			break;
		}
		case NODE_VARIABLE_DECLARATION:
		{
			Symbol variable = { node->variable_declaration.type->string.value, false, NULL };
			generator->env->symbol_table.set(node->variable_declaration.name->string.value, variable);
			break;
		}
		case NODE_BLOCK:
		{
			codegen_statement(generator, node->block.left);
			break;
		}
		case NODE_UNARY_OPERATOR:
		case NODE_BINARY_OPERATOR:
			codegen_expression(generator, node);
			break;
		case NODE_FUNCTION_SIGNATURE:
		case NODE_FUNCTION:
			codegen_function(generator, node);
			break;
		case NODE_FUNCTION_CALL:
			codegen_expression(generator, node);
			break;
		case NODE_IF:
		{
			// TODO: Switch to using more proper boolean comparison?
			Value *cond = codegen_expression(generator, node->conditional.condition);
			cond = builder.CreateICmpNE(cond, ConstantInt::get(getGlobalContext(), APInt(32, 0)), "ifcond");

			Function *function = builder.GetInsertBlock()->getParent();
			BasicBlock *then = BasicBlock::Create(getGlobalContext(), "then", function);
			BasicBlock *other = BasicBlock::Create(getGlobalContext(), "else", function);
			BasicBlock *merge = BasicBlock::Create(getGlobalContext(), "ifcont", function);
			builder.CreateCondBr(cond, then, other);

			// 'Then' block
			builder.SetInsertPoint(then);
			codegen_statement(generator, node->conditional.then);
			builder.CreateBr(merge);
			then = builder.GetInsertBlock();

			// 'Else' block
			builder.SetInsertPoint(other);
			if (node->conditional.other)
				codegen_statement(generator, node->conditional.other);
			builder.CreateBr(merge);
			other = builder.GetInsertBlock();

			// TODO: Is returning from inside blocks an issue? It doesn't seem to be,
			// but also I've seen people use phi nodes for this kind of purpose.
			// Can we rely on an optimise pass to make that happen? Hmm..

			// 'Merge' block
			builder.SetInsertPoint(merge);
			break;
		}
		case NODE_WHILE_LOOP:
		{
			Function *function = builder.GetInsertBlock()->getParent();
			BasicBlock *preloop = BasicBlock::Create(getGlobalContext(), "prewhile", function);
			BasicBlock *loop = BasicBlock::Create(getGlobalContext(), "while", function);
			BasicBlock *after = BasicBlock::Create(getGlobalContext(), "afterwhile", function);
			builder.CreateBr(preloop);

			builder.SetInsertPoint(preloop);
			// TODO: Switch to using more proper boolean comparison?
			Value *cond = codegen_expression(generator, node->conditional.condition);
			cond = builder.CreateICmpNE(cond, ConstantInt::get(getGlobalContext(), APInt(32, 0)), "whilecond");
			builder.CreateCondBr(cond, loop, after);

			// TODO: Handle 'other' branch (while..else)

			builder.SetInsertPoint(loop);
			codegen_statement(generator, node->conditional.then);
			builder.CreateBr(preloop);

			builder.SetInsertPoint(after);
			break;
		}
		case NODE_RETURN:
		{
			Value *val = codegen_expression(generator, node->unary_operator.operand);
			builder.CreateRet(val);
			break;
		}
		case NODE_DO_LOOP:
		case NODE_UNTIL_LOOP:
		case NODE_FOR_LOOP:
		case NODE_BREAK:
		case NODE_CONTINUE:
			codegen_error(generator, "feature not yet implemented!");
			break;
		case NODE_CONSTANT_INT:
		case NODE_CONSTANT_FLOAT:
		case NODE_CONSTANT_STRING:
			break;
		default:
			codegen_error(generator, "failed to generate statement for ASTNode");
			break;
	}
}

void codegen(CodeGenerator *generator) {
	generator->module = new Module("program", getGlobalContext());

	// TODO: Add module metadata here?

	codegen_statement(generator, generator->root);
	verifyModule(*generator->module);

	// TODO: Insert numerous code passes here (various optimisations, etc.)

	generator->module->dump();
	delete generator->module;
}
