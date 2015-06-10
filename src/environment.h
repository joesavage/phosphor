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
	PBaseType *base_type; // TODO: We'll never use both of these at the same time
	PType *indirect_type;

	bool is_pointer; // TODO: We'll never use both of these at the same time
	unsigned int array_size;
	// TODO: Other modifiers (possibly bitflag all modifiers in future)

	PType(PBaseType *base_type = NULL, bool is_pointer = 0,
	      unsigned int array_size = 0, PType *indirect_type = NULL) {
		this->base_type = base_type;
		this->is_pointer = is_pointer;
		this->array_size = array_size;
		this->indirect_type = indirect_type;
	}

	Type *getLLVMType() {
		Type *result = NULL;

		if (is_pointer) {
			assert(indirect_type);
			result = PointerType::get(indirect_type->getLLVMType(), 0);
		} else if (array_size > 0) {
			assert(indirect_type);
			result = ArrayType::get(indirect_type->getLLVMType(), array_size);
		} else {
			result = base_type->llvmty;
		}

		// Other modifiers can go here

		return result;
	}

	PBaseType *getBaseType() {
		if (is_pointer || array_size > 0)
			return indirect_type->getBaseType();
		return base_type;
	}

	char *to_string() {
		if ((!base_type || !base_type->name) && !indirect_type)
			return "(unknown)";

		size_t buf_len = 255;
		char *result = (char *)heap_alloc(buf_len + 1);
		result[buf_len - 1] = '\0';

		if (is_pointer || array_size > 0) {
			char *indirect_ty = indirect_type->to_string();
			strncpy(result, indirect_ty, buf_len);

			if (is_pointer)
				strncpy(result + strlen(indirect_ty), "^", buf_len - strlen(indirect_ty));
			else if (array_size > 0) // TODO: Include array size in output
				strncpy(result + strlen(indirect_ty), "[]", buf_len - strlen(indirect_ty));
		} else {
			strncpy(result, base_type->name, buf_len);
		}

		// Can handle other modifiers here

		return result;
	}

	bool operator==(const PType &ty) {
		return ty.base_type == base_type
		    && ty.is_pointer == is_pointer
		    && ty.array_size == array_size
		    && ((!ty.indirect_type && !indirect_type)
		     || (*ty.indirect_type == *indirect_type));
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
