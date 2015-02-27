#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"

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

// TODO: Function argument types should live in here one day. Similarly, one day
// function overloading support should be present.
struct PFunction {
	char *return_type;
	Function *value;

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
	HashMap<PValue> symbol_table;
	HashMap<PFunction> function_table;

	Environment *parent;

	Environment() { parent = NULL; }
};

PValue *search_for_symbol(Environment env, char *name);
PType *search_for_type(Environment env, char *name);
PFunction *search_for_function(Environment env, char *name);

#endif
