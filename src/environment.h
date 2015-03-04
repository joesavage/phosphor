#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

#include "hashmap.hpp"
#include "memorylist.hpp"

using namespace llvm;

struct PType {
	Type *type;
	bool is_signed; // TODO: Switch to type-specific 'flags' (bitmask).

	PType() {
		type = NULL;
		is_signed = false;
	}

	PType(Type *type, bool is_signed = false) {
		this->type = type;
		this->is_signed = is_signed;
	}

	bool operator==(const PType &ty) {
		return ty.type == type
		    && ty.is_signed == is_signed;
	}
	bool operator!=(const PType &ty) { return !(*this == ty); }
};


struct PValue {
	char *type;
	Value *value;

	PValue() {
		type = NULL;
		value = NULL;
	}

	PValue(char *type, Value *value) {
		this->type = type;
		this->value = value;
	}
};

struct PVariable {
	char *type;
	AllocaInst *value;

	PVariable() {
		type = NULL;
		value = NULL;
	}

	PVariable(char *type, AllocaInst *value) {
		this->type = type;
		this->value = value;
	}
};

// TODO: Function overloading should be supported here one day.
struct PFunction {
	char *return_type;
	Function *value;
	MemoryList<char *> arg_types;
	// TODO: Some state to show whether this is a binary/unary operator (or not).

	PFunction() {
		return_type = NULL;
		value = NULL;
	}

	PFunction(char *return_type, Function *value) {
		this->return_type = return_type;
		this->value = value;
	}
};

struct Environment {
	HashMap<PType> type_table;
	HashMap<PVariable> symbol_table;
	HashMap<PFunction> function_table;

	char *current_function;
	Environment *parent;

	Environment() { parent = NULL; current_function = NULL; }
};

PVariable *search_for_symbol(Environment env, char *name);
PType *search_for_type(Environment env, char *name);
PFunction *search_for_function(Environment env, char *name);

#endif
