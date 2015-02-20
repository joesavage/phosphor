#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"

#include "hashmap.hpp"

using namespace llvm;

struct Symbol {
	char *type;
	bool function;
	llvm::Value *value;
};

struct Environment {
	HashMap<Symbol> symbol_table;
	HashMap<Type *> type_table;

	Environment *parent;

	Environment() { parent = NULL; }
};

HashNode<Symbol> *search_for_symbol(Environment env, char *name);
HashNode<Type *> *search_for_type(Environment env, char *name);

#endif
