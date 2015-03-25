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
	Type *llvmty;

	// TODO: Switch to bitmask flags?
	bool is_numeric;
	bool is_signed;
	bool is_float;
	size_t numbits;

	PType() {
		llvmty = NULL;
		is_numeric = false;
		is_signed = false;
		is_float = false;
		numbits = 0;
	}

	PType(Type *llvmty, size_t numbits = 0, bool is_numeric = false,
	      bool is_float = false, bool is_signed = false) {
		this->llvmty = llvmty;
		this->is_numeric = is_numeric;
		this->is_float = is_float;
		this->is_signed = is_signed;
		this->numbits = numbits;
	}

	bool operator==(const PType &ty) {
		return ty.llvmty == llvmty
		    && ty.is_numeric == is_numeric
		    && ty.is_float == is_float
		    && ty.is_signed == is_signed
		    && ty.numbits == numbits;
	}
	bool operator!=(const PType &ty) { return !(*this == ty); }
};

struct PExType {
	char *type_name;
	bool is_pointer;
	// TODO: Other modifiers (possibly bitflag all modifiers in future)

	PExType(char *type_name = NULL, bool is_pointer = false) {
		this->type_name = type_name;
		this->is_pointer = is_pointer;
	}

	inline bool is_set() {
		return type_name;
	}

	char *to_string() {
		if (!type_name)
			return NULL;

		size_t buf_len = 255;
		char *result = (char *)heap_alloc(buf_len);
		strncpy(result, type_name, buf_len);
		if (is_pointer)
			strncpy(result + strlen(type_name), "^", buf_len);

		// Can handle other modifiers here

		return result;
	}

	bool operator==(const PExType &ty) {
		return !strcmp(ty.type_name, type_name)
		    && ty.is_pointer == is_pointer;
	}
	bool operator!=(const PExType &ty) { return !(*this == ty); }
};


struct PValue {
	PExType type;
	Value *llvmval;

	PValue() {
		type = NULL;
		llvmval = NULL;
	}

	PValue(PExType type, Value *llvmval) {
		this->type = type;
		this->llvmval = llvmval;
	}
};

struct PVariable {
	PExType type;
	AllocaInst *llvmval;

	PVariable() {
		type = NULL;
		llvmval = NULL;
	}

	PVariable(char *type, AllocaInst *llvmval) {
		this->type = type;
		this->llvmval = llvmval;
	}
};

// TODO: Function overloading should be supported here one day.
struct PFunction {
	PExType return_type;
	Function *llvmval;
	MemoryList<PExType> arg_types;
	// TODO: Some state to show whether this is a binary/unary operator (or not).

	PFunction() {
		return_type = NULL;
		llvmval = NULL;
	}

	PFunction(char *return_type, Function *llvmval) {
		this->return_type = return_type;
		this->llvmval = llvmval;
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
