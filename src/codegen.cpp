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
				// TODO: Variables shouldn't really be handled like this, like, at all
				// but mutability is hard (phi nodes in pure SSA) - need to alloc mem.
				Value *val = codegen_expression(generator, node->binary_operator.right);
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

			if (left->getType() != IntegerType::get(getGlobalContext(), 64)) {
				// Right now, we only have hardcoded support for (64-bit) integers.
				codegen_error(generator, "unsupported type for binary operation");
				break;
			}

			if (!strcmp(node->string.value, "+")) {
				result = builder.CreateAdd(left, right, "addtmp");
				builder.CreateRet(result); // TODO: Remove this debug line
			} else if (!strcmp(node->string.value, "*")) {
				result = builder.CreateMul(left, right, "multmp");
			}
			break;
		}
		case NODE_CONSTANT_INT:
		{
			// TODO: Types aren't 64-bit by default for any reason right now.
			// Just an arbitrary decision that needs reviewing in future.
			//
			// TODO: If we want an int literal max size instead of just wrapping,
			// we should add that here.
			//
			// TODO: Need to deal with hex, etc. (probably earlier than this stage of
			// compilation).
			return ConstantInt::get(getGlobalContext(), APInt(64, StringRef(node->string.value), 10));
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
			builder.CreateRet(val); // TODO: Remove this debug line
			return val;
		}
		default:
			codegen_error(generator, "failed to generate expression for ASTNode");
			break;
	}

	return result;
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
		case NODE_EXPRESSION_LIST:
			codegen_statement(generator, node->skeleton.left);
			if (node->skeleton.right)
				codegen_statement(generator, node->skeleton.right);
			break;
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
		{
			// Environment *previous_env = generator->env;
			// generator->env = node->function_signature.env;

			ASTNode **current = &node->function_signature.args;
			std::vector<Type *> args;
			do {
				codegen_statement(generator, *current);
				ASTNode *declaration = (*current)->skeleton.left;
				Type *type = search_for_type(*generator->env, declaration->variable_declaration.type->string.value)->value;
				args.push_back(type);
			} while((*current = (*current)->skeleton.right));

			FunctionType *function_type = FunctionType::get(search_for_type(*generator->env, node->function_signature.type->string.value)->value, args, false);
			Function *function = Function::Create(function_type, Function::ExternalLinkage, node->function_signature.name->string.value, generator->module);
			function->dump();

			// generator->env = previous_env;
			break;
		}
		case NODE_FUNCTION:
		case NODE_FUNCTION_CALL:
		case NODE_IF:
		case NODE_DO_LOOP:
		case NODE_WHILE_LOOP:
		case NODE_UNTIL_LOOP:
		case NODE_FOR_LOOP:
		case NODE_BREAK:
		case NODE_CONTINUE:
		case NODE_RETURN:
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

	// TODO: Remove
	FunctionType *func_type = FunctionType::get(Type::getVoidTy(getGlobalContext()), false);
	Function *func = Function::Create(func_type, Function::ExternalLinkage, "main", generator->module);
	BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", func);
	builder.SetInsertPoint(BB);

	codegen_statement(generator, generator->root);
	verifyModule(*generator->module);

	generator->module->dump();
	delete generator->module;
}
