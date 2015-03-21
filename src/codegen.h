#ifndef CODEGEN_H
#define CODEGEN_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/PassManager.h"

#include "ast.h"
#include "environment.h"

using namespace llvm;

struct CodeGenerator {
	ASTNode *root;
	char *error;
	ASTNode *errnode;
	Environment *env;
	Module *module;
	IRBuilder<> *builder;
	FunctionPassManager *fpm;

	void generate();
private:
	void generate_statement(ASTNode *node);
	PVariable generate_variable_declaration(ASTNode *node);
	PValue generate_expression(ASTNode *node);
	PFunction generate_function(ASTNode *node);
	PType lookup_type(PValue value);
	PType lookup_type(char *value);
	PFunction lookup_function(char *name);
	PVariable lookup_symbol(char *name);
	PValue get_boolean_value(bool value);
	bool implicit_type_convert(PValue *source, char *dest_typename);
	bool explicit_type_convert(PValue *source, char *dest_typename);
	AllocaInst *create_entry_block_alloca(char *name, Type *type);
	void set_error(ASTNode *node, const char *format, ...);
};

#endif
