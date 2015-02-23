#include "environment.h"

HashNode<Symbol> *search_for_symbol(Environment env, char *name) {
	Environment *current = &env;
	HashNode<Symbol> *result;
	do {
		if ((result = current->symbol_table[name]))
			return result;
	} while ((current = current->parent));

	return NULL;
}

HashNode<Type *> *search_for_type(Environment env, char *name) {
	Environment *current = &env;
	HashNode<Type *> *result;
	do {
		if ((result = current->type_table[name]))
			return result;
	} while ((current = current->parent));

	return NULL;
}
