#ifndef CODEGEN_H
#define CODEGEN_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/PassManager.h"

#include "ast.h"
#include "environment.h"
#include "memoryarena.h"

using namespace llvm;

struct CodeGenerator {
	ASTNode *root;
	char *error;
	ASTNode *errnode;
	MemoryArena *memory;
	Environment *env;
	Module *module;
	IRBuilder<> *builder;
	FunctionPassManager *fpm;

	Module *generate(int optimisation_level);
private:
	void generate_statement(ASTNode *node);
	PVariable generate_lvalue(ASTNode *node, bool internal = false);
	PValue generate_rvalue(ASTNode *node);
	PFunction generate_function(ASTNode *node);
	PBaseType *lookup_base_type(const char *name);
	PFunction *lookup_function(const char *name);
	PVariable *lookup_symbol(const char *name);
	PValue get_boolean_value(bool value);
	bool implicit_type_convert(PValue *source, PType dest_extype);
	bool explicit_type_convert(PValue *source, PType dest_extype);
	AllocaInst *create_entry_block_alloca(char *name, Type *type);
	void set_error(ASTNode *node, const char *format, ...);
};

#endif
