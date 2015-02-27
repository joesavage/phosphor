#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/CallingConv.h"

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

static PType lookup_type(CodeGenerator *generator, PValue value) {
	PType *type = search_for_type(*generator->env, value.type);
	return type ? *type : PType();
}

static PType lookup_type(CodeGenerator *generator, char *value) {
	PType *type = search_for_type(*generator->env, value);
	return type ? *type : PType();
}

static PFunction lookup_function(CodeGenerator *generator, char *name) {
	PFunction *function = search_for_function(*generator->env, name);
	return function ? *function : PFunction();
}

// NOTE: Keeping pointers to these is dangerous as if the HashMap resizes it'll
// cause pointer invalidation.
static PValue *lookup_symbol(CodeGenerator *generator, char *symbol) {
	return search_for_symbol(*generator->env, symbol);
}


static PValue codegen_expression(CodeGenerator *generator, ASTNode *node) {
	PValue result;

	switch (node->type) {
		case NODE_BINARY_OPERATOR:
		{
			DECL_ASTNODE_DATA(node, binary_operator, pnode)

			if (!strcmp(pnode.value, "=")) {
				DECL_ASTNODE_DATA(pnode.left, string, symbol_name)
				PValue *var_sym = lookup_symbol(generator, symbol_name.value);
				PValue value = codegen_expression(generator, pnode.right);

				if (lookup_type(generator, value) != lookup_type(generator, *var_sym)) {
					// TODO: Type conversions (factor out) - for integers, using Trunc or ZExt/SExt (getIntegerBitWidth).
					codegen_error(generator, "type mismatch in assignment");
					break;
				}

				// TODO: This side-effect means that all assignment expressions end up being immediately executed.
				// This is obviously less than ideal, and so we need to implement mutable state by allocating memory.
				var_sym->value = value.value;
				break;
			}


			PValue left = codegen_expression(generator, pnode.left);
			PValue right = codegen_expression(generator, pnode.right);

			if (lookup_type(generator, left) != lookup_type(generator, right)) {
				codegen_error(generator, "type mismatch in addition operation!");
				break;
			}

			// Right now, we only have hardcoded support for (32-bit) integers.
			if (lookup_type(generator, left).type != IntegerType::get(getGlobalContext(), 32)) {
				codegen_error(generator, "unsupported type for binary operation");
				break;
			}

			result.type = left.type;
			if (!strcmp(pnode.value, "==")) {
				result.type = "bool";
				result.value = builder.CreateICmpEQ(left.value, right.value, "eqtmp");
			} else if (!strcmp(pnode.value, "!=")) {
				result.type = "bool";
				result.value = builder.CreateICmpNE(left.value, right.value, "eqtmp");
			} else if (!strcmp(pnode.value, "+")) {
				result.value = builder.CreateAdd(left.value, right.value, "addtmp");
			} else if (!strcmp(pnode.value, "*")) {
				result.value = builder.CreateMul(left.value, right.value, "multmp");
			} else if (!strcmp(pnode.value, "/")) {
				// TODO: Handle unsigned
				result.value = builder.CreateSDiv(left.value, right.value, "divtmp");
			} else if (!strcmp(pnode.value, "-")) {
				result.value = builder.CreateSub(left.value, right.value, "subtmp");
			}
			break;
		}
		case NODE_FUNCTION_CALL:
		{
			DECL_ASTNODE_DATA(node, function_call, pnode)
			char *function_name = pnode.name->data.string.value;
			Function *function = generator->module->getFunction(function_name);
			if (!function) {
				codegen_error(generator, "unknown function referenced '%s'", function_name);
				break;
			}

			size_t args_count = pnode.args.size();
			MemoryList<Value *> args(args_count);
			for (size_t i = 0; i < args_count; ++i)
				args.add(codegen_expression(generator, pnode.args[i]).value);

			// TODO: Check arguments match the signature! (no. and type)

			PFunction pfunction = lookup_function(generator, function_name);
			if (!pfunction.return_type) {
				codegen_error(generator, "failed to find function '%s'", function_name);
				break;
			}

			result.type = pfunction.return_type;
			result.value = builder.CreateCall(function, ArrayRef<Value *>(args.getPointer(0), args_count), "calltmp");
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
			DECL_ASTNODE_DATA(node, string, pnode)
			result.type = "int32";
			result.value = ConstantInt::get(getGlobalContext(), APInt(32, StringRef(pnode.value), 10));
			break;
		}
		case NODE_CONSTANT_BOOL:
		{
			DECL_ASTNODE_DATA(node, integer, pnode)
			result.type = "bool";
			// TODO: Use the type table information to construct (e.g. numbits) rather
			// than duplicating it.
			result.value = ConstantInt::get(getGlobalContext(), APInt(1, pnode.value));
			break;
		}
		case NODE_CONSTANT_FLOAT:
		{
			// TODO: This seems like a pretty terrible way to initialize an APFloat.
			// TODO: Also, we probably want to deal with oversized floats or whatever
			// here.
			DECL_ASTNODE_DATA(node, string, pnode)
			result.type = "float32";
			APFloat number(0.0);
			number.convertFromString(pnode.value, APFloat::rmNearestTiesToEven);
			result.value = ConstantFP::get(getGlobalContext(), number);
			break;
		}
		case NODE_IDENTIFIER:
		{
			DECL_ASTNODE_DATA(node, string, pnode)
			PValue *value = lookup_symbol(generator, pnode.value);
			if (!value)
				codegen_error(generator, "failed to find symbol '%s'", pnode.value);
			return *value;
		}
		default:
			codegen_error(generator, "failed to generate expression for ASTNode");
			break;
	}

	return result;
}

static PFunction codegen_function(CodeGenerator *generator, ASTNode *node) {
	PFunction result;

	switch (node->type) {
		case NODE_FUNCTION_SIGNATURE:
		{
			DECL_ASTNODE_DATA(node, function_signature, pnode)
			Environment *previous_env = generator->env;
			generator->env = pnode.env;

			size_t args_count = pnode.args.size();
			MemoryList<Type *> args(args_count);
			for (size_t i = 0; i < args_count; ++i) {
				DECL_ASTNODE_DATA(pnode.args[i], variable_declaration, arg)
				codegen_statement(generator, pnode.args[i]);
				PType type = lookup_type(generator, arg.type->data.string.value);
				args.add(type.type);
			}

			// TODO: Needle to handle redefinitions
			char *function_name = pnode.name->data.string.value;
			char *function_type_name = pnode.type->data.string.value;
			FunctionType *function_type = FunctionType::get(lookup_type(generator, function_type_name).type, ArrayRef<Type *>(args.getPointer(0), args_count), false);
			Function *function = Function::Create(function_type, Function::ExternalLinkage, function_name, generator->module);
			function->addFnAttr(Attribute::NoUnwind);

			// If the name we got back isn't the one we assigned, there was a conflict
			if (function->getName() != function_name) {
				// Erase the just-created signature, and get the previous one.
				function->eraseFromParent();
				function = generator->module->getFunction(function_name);

				if (!function->empty()) {
					codegen_error(generator, "redefinition of function is not allowed");
					break;
				}

				PFunction pfunction = lookup_function(generator, function_name);
				if (!pfunction.return_type) {
					codegen_error(generator, "conflict between LLVM and function table state");
					break;
				}

				// TODO: Function argument check
				result = pfunction;
			} else {
				result.value = function;
				result.return_type = pnode.type->data.string.value;
				previous_env->function_table.set(function_name, result);
			}

			generator->env = previous_env;
			break;
		}
		case NODE_FUNCTION:
		{
			DECL_ASTNODE_DATA(node, function, pnode)
			PFunction function = codegen_function(generator, pnode.signature);
			BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", function.value);
			builder.SetInsertPoint(BB);

			Environment *prev_env = generator->env; // TODO: Helper for env push/pop
			generator->env = pnode.signature->data.function_signature.env;

			codegen_statement(generator, pnode.body);

			generator->env = prev_env;

			result = function;
			break;
		}
		default:
			codegen_error(generator, "unexpected token for function code generation");
			break;
	}

	return result;
}

static void codegen_statement(CodeGenerator *generator, ASTNode *node) {
	switch (node->type) {
		case NODE_STATEMENTS:
		{
			Environment *previous_env = generator->env;
			generator->env = node->data.block.env;
			do {
				if (node->data.block.left)
					codegen_statement(generator, node->data.block.left);
				if (generator->error)
					return;
			} while ((node = node->data.block.right));
			generator->env = previous_env;
			break;
		}
		case NODE_VARIABLE_DECLARATION:
		{
			DECL_ASTNODE_DATA(node, variable_declaration, pnode)
			PValue variable(pnode.type->data.string.value, NULL);
			generator->env->symbol_table.set(pnode.name->data.string.value, variable);
			break;
		}
		case NODE_BLOCK:
		{
			DECL_ASTNODE_DATA(node, block, pnode)
			codegen_statement(generator, pnode.left);
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
			DECL_ASTNODE_DATA(node, conditional, pnode)
			PValue cond = codegen_expression(generator, pnode.condition);

			if (lookup_type(generator, cond) != lookup_type(generator, "bool")) {
				codegen_error(generator, "unsupported type for if statement condition");
				break;
			}

			cond.value = builder.CreateICmpNE(cond.value, ConstantInt::get(getGlobalContext(), APInt(1, 0)), "ifcond");

			Function *function = builder.GetInsertBlock()->getParent();
			BasicBlock *then = BasicBlock::Create(getGlobalContext(), "then", function);
			BasicBlock *other = BasicBlock::Create(getGlobalContext(), "else", function);
			BasicBlock *merge = BasicBlock::Create(getGlobalContext(), "ifcont", function);
			builder.CreateCondBr(cond.value, then, other);

			// 'Then' block
			builder.SetInsertPoint(then);
			codegen_statement(generator, pnode.then);
			builder.CreateBr(merge);
			then = builder.GetInsertBlock();

			// 'Else' block
			builder.SetInsertPoint(other);
			if (pnode.other)
				codegen_statement(generator, pnode.other);
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
			DECL_ASTNODE_DATA(node, conditional, pnode)
			Function *function = builder.GetInsertBlock()->getParent();
			BasicBlock *preloop = BasicBlock::Create(getGlobalContext(), "prewhile", function);
			BasicBlock *loop = BasicBlock::Create(getGlobalContext(), "while", function);
			BasicBlock *after = BasicBlock::Create(getGlobalContext(), "afterwhile", function);
			builder.CreateBr(preloop);

			builder.SetInsertPoint(preloop);

			PValue cond = codegen_expression(generator, pnode.condition);

			if (lookup_type(generator, cond) != lookup_type(generator, "bool")) {
				codegen_error(generator, "unsupported type for while statement condition");
				break;
			}

			cond.value = builder.CreateICmpNE(cond.value, ConstantInt::get(getGlobalContext(), APInt(1, 0)), "whilecond");
			builder.CreateCondBr(cond.value, loop, after);

			// TODO: Handle 'other' branch (while..else)

			builder.SetInsertPoint(loop);
			codegen_statement(generator, pnode.then);
			builder.CreateBr(preloop);

			builder.SetInsertPoint(after);
			break;
		}
		case NODE_RETURN:
		{
			DECL_ASTNODE_DATA(node, unary_operator, pnode)
			PValue val = codegen_expression(generator, pnode.operand);
			// TODO: Type check
			builder.CreateRet(val.value);
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
