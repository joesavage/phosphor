#include <stddef.h>

#include "ast.h"

ASTNode::ASTNode() {
	// TODO: Better way to initialize all union members to 0?
	function_signature.name = NULL;
	function_signature.type = NULL;
	function_signature.args = NULL;
	function_signature.env = NULL;
}
