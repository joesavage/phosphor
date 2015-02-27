#include <stddef.h>

#include "ast.h"

ASTNode::ASTNode(ASTNodeType type) {
	this->type = type;

	// NODE: Remember that just modifying the 'type' after this init may result
	// in uninitialized union members.

	switch(type) {
		case NODE_BLOCK:
		case NODE_STATEMENTS:
			block.left = NULL;
			block.right = NULL;
			block.env = NULL;
			break;
		case NODE_VARIABLE_DECLARATION:
			variable_declaration.type = NULL;
			variable_declaration.name = NULL;
			variable_declaration.action = NULL;
			break;
		case NODE_IF:
		case NODE_DO_LOOP:
		case NODE_WHILE_LOOP:
		case NODE_UNTIL_LOOP:
			conditional.condition = NULL;
			conditional.then = NULL;
			conditional.other = NULL;
			break;
		case NODE_FUNCTION:
			function.signature = NULL;
			function.body = NULL;
			break;
		case NODE_FUNCTION_SIGNATURE:
			function_signature.name = NULL;
			function_signature.type = NULL;
			function_signature.args = MemoryList<ASTNode *>();
			function_signature.env = NULL;
			break;
		case NODE_FUNCTION_CALL:
			function_call.name = NULL;
			function_call.args = MemoryList<ASTNode *>();
			break;
		case NODE_FOR_LOOP:
		case NODE_BREAK:
		case NODE_CONTINUE:
		case NODE_RETURN:
			break;
		case NODE_UNARY_OPERATOR:
			unary_operator.value = NULL;
			unary_operator.operand = NULL;
			break;
		case NODE_BINARY_OPERATOR:
			binary_operator.value = NULL;
			binary_operator.left = NULL;
			binary_operator.right = NULL;
			break;
		case NODE_CONSTANT_BOOL:
			integer.value = 0;
			break;
		case NODE_IDENTIFIER:
		case NODE_TYPE:
		case NODE_CONSTANT_INT:
		case NODE_CONSTANT_FLOAT:
		case NODE_CONSTANT_STRING:
			string.value = NULL;
			break;
	}
}
