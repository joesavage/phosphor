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

typedef enum {
	NOTHING,
	CAST,
	CAST_AFTER_SIGNED_PROMOTION
} implicit_type_convert_hint;

typedef enum {
	FIRST_IMPLICITLY_CASTS_TO_SECOND,
	SECOND_IMPLICITLY_CASTS_TO_FIRST,
	FIRST_IMPLICITLY_CASTS_TO_SIGNED_PROMOTED_SECOND,
	SECOND_IMPLICITLY_CASTS_TO_SIGNED_PROMOTED_FIRST,
	ARRAY_POINTERS_CAST_TO_FIRST_ELEMENT_POINTERS
} shared_type_convert_hint;

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
	bool can_cast(PType source_type, PType dest_type);
	bool create_cast(PValue *source, PType dest_type);

	bool can_promote_to_signed(PType type, PType *target = NULL);
	bool promote_to_signed(PValue *value);
	bool can_implicit_type_convert(PType source_type, PType dest_type,
	                               implicit_type_convert_hint *hint = NULL);
	bool implicit_type_convert(PValue *source, PType dest_extype,
	                           bool maintain_malleability = false);
	bool explicit_type_convert(PValue *source, PType dest_extype);
	bool can_convert_to_shared_type(PType first, PType second,
	                                shared_type_convert_hint *hint = NULL);
	bool convert_to_shared_type(PValue *first, PValue *second);
	Value *create_variable(char *name, Type *type, Value *init, bool constant);
	void set_error(ASTNode *node, const char *format, ...);
};

#endif
