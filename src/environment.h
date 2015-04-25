#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

#include "hashmap.hpp"
#include "memorylist.hpp"

using namespace llvm;

struct PBaseType {
	const char *name;
	Type *llvmty;

	// TODO: Switch to bitmask flags?
	bool is_numeric;
	bool is_signed;
	bool is_float;
	size_t numbits;

	PBaseType() {
		name = NULL;
		llvmty = NULL;
		is_numeric = false;
		is_signed = false;
		is_float = false;
		numbits = 0;
	}

	PBaseType(const char *name, Type *llvmty, size_t numbits = 0,
	          bool is_numeric = false, bool is_float = false,
	          bool is_signed = false)
	{
		this->name = name;
		this->llvmty = llvmty;
		this->is_numeric = is_numeric;
		this->is_float = is_float;
		this->is_signed = is_signed;
		this->numbits = numbits;
	}
};

struct PType {
	PBaseType *base_type;
	unsigned int pointer_level;
	// TODO: Other modifiers (possibly bitflag all modifiers in future)

	PType(PBaseType *base_type = NULL, int pointer_level = 0) {
		this->base_type = base_type;
		this->pointer_level = pointer_level;
	}

	Type *getLLVMType() {
		Type *result = base_type->llvmty;

		for (unsigned int i = 0; i < pointer_level; ++i)
			result = PointerType::get(result, 0);

		// Other modifiers can go here

		return result;
	}

	char *to_string() {
		if (!base_type || !base_type->name)
			return "(unknown)";

		const char *type_name = base_type->name;
		size_t buf_len = 255;
		char *result = (char *)heap_alloc(buf_len);
		strncpy(result, type_name, buf_len);
		for (unsigned int i = 0; i < pointer_level; ++i)
			strncpy(result + strlen(type_name) + i,
			        "^",
			        buf_len - strlen(type_name) - i);

		// Can handle other modifiers here

		return result;
	}

	bool operator==(const PType &ty) {
		return ty.base_type == base_type
		    && ty.pointer_level == pointer_level;
	}
	bool operator!=(const PType &ty) { return !(*this == ty); }
};


struct PValue {
	PType type;
	Value *llvmval;

	PValue() {
		type = NULL;
		llvmval = NULL;
	}

	PValue(PType type, Value *llvmval) {
		this->type = type;
		this->llvmval = llvmval;
	}
};

struct PVariable {
	PType type;
	AllocaInst *llvmval;

	PVariable() {
		type = NULL;
		llvmval = NULL;
	}

	PVariable(PType type, AllocaInst *llvmval) {
		this->type = type;
		this->llvmval = llvmval;
	}
};

// TODO: Function overloading should be supported here one day?
struct PFunction {
	PType return_type;
	Function *llvmval;
	MemoryList<PType> arg_types;
	// TODO: Some state to show whether this is a binary/unary operator (or not).

	PFunction() {
		return_type = NULL;
		llvmval = NULL;
	}

	PFunction(PType return_type, Function *llvmval) {
		this->return_type = return_type;
		this->llvmval = llvmval;
	}
};

struct Environment {
	HashMap<PBaseType *> type_table;
	HashMap<PVariable> symbol_table;
	HashMap<PFunction> function_table;

	char *current_function;
	Environment *parent;

	Environment() { parent = NULL; current_function = NULL; }
};

PVariable *search_for_symbol(Environment env, const char *name);
PBaseType *search_for_type(Environment env, const char *name);
PFunction *search_for_function(Environment env, const char *name);

#endif
