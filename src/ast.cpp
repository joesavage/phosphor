#include <stddef.h>

#include "ast.h"

ASTNode::ASTNode() {
	// TODO: Better way to initialize all union members to 0?
	// (I'm super worried that this will break)
	variable_declaration.type = NULL;
	variable_declaration.name = NULL;
	variable_declaration.action = NULL;
}
