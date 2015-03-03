#ifndef CODEGEN_H
#define CODEGEN_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

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

	void generate();
private:
	void generate_statement(ASTNode *node);
	PValue generate_expression(ASTNode *node);
	PFunction generate_function(ASTNode *node);
	PType lookup_type(PValue value);
	PType lookup_type(char *value);
	PFunction lookup_function(char *name);
	PValue *lookup_symbol(char *symbol);
	PValue get_boolean_value(bool value);
	void set_error(ASTNode *node, const char *format, ...);
};

#endif
