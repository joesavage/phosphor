#include "environment.h"
#include "helpers.h"

PVariable *search_for_symbol(Environment env, char *name) {
	Environment *current = &env;
	HashNode<PVariable> *result;
	do {
		if ((result = current->symbol_table[name]))
			return &result->value;
	} while ((current = current->parent));

	return NULL;
}

PBaseType *search_for_type(Environment env, char *name) {
	Environment *current = &env;
	HashNode<PBaseType *> *result;
	do {
		if ((result = current->type_table[name]))
			return result->value;
	} while ((current = current->parent));

	return NULL;
}

PFunction *search_for_function(Environment env, char *name) {
	Environment *current = &env;
	HashNode<PFunction> *result;
	do {
		if ((result = current->function_table[name]))
			return &result->value;
	} while ((current = current->parent));

	return NULL;
}
