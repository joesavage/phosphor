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

	Module *generate();
private:
	void generate_statement(ASTNode *node);
	PVariable generate_lvalue(ASTNode *node);
	PValue generate_rvalue(ASTNode *node);
	PFunction generate_function(ASTNode *node);
	PBaseType *lookup_base_type(char *name);
	PFunction lookup_function(char *name);
	PVariable lookup_symbol(char *name);
	PValue get_boolean_value(bool value);
	bool implicit_type_convert(PValue *source, PType dest_extype);
	bool explicit_type_convert(PValue *source, PType dest_extype);
	AllocaInst *create_entry_block_alloca(char *name, Type *type);
	void set_error(ASTNode *node, const char *format, ...);
};

#endif
