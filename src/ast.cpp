#include <stddef.h>

#include "ast.h"

void initialise_node(ASTNode *node, ASTNodeType type) {
	node->type = type;

	// NODE: Remember that just modifying the 'type' after this init may result
	// in uninitialized union members.

	switch(type) {
		case NODE_STATEMENTS:
			node->data.statements.children = MemoryList<ASTNode *>();
			break;
		case NODE_BLOCK:
			node->data.block.statements = NULL;
			node->data.block.env = NULL;
			break;
		case NODE_VARIABLE_DECLARATION:
			node->data.variable_declaration.type = NULL;
			node->data.variable_declaration.name = NULL;
			node->data.variable_declaration.init = NULL;
			break;
		case NODE_IF:
		case NODE_DO_LOOP:
		case NODE_WHILE_LOOP:
		case NODE_UNTIL_LOOP:
			node->data.conditional.condition = NULL;
			node->data.conditional.then = NULL;
			node->data.conditional.otherwise = NULL;
			break;
		case NODE_FUNCTION:
			node->data.function.signature = NULL;
			node->data.function.body = NULL;
			break;
		case NODE_FUNCTION_SIGNATURE:
			node->data.function_signature.name = NULL;
			node->data.function_signature.type = NULL;
			node->data.function_signature.args = MemoryList<ASTNode *>();
			node->data.function_signature.env = NULL;
			break;
		case NODE_FUNCTION_CALL:
			node->data.function_call.name = NULL;
			node->data.function_call.args = MemoryList<ASTNode *>();
			break;
		case NODE_FOR_LOOP:
		case NODE_BREAK:
		case NODE_CONTINUE:
		case NODE_RETURN:
			break;
		case NODE_UNARY_OPERATOR:
		case NODE_CAST_OPERATOR:
			node->data.unary_operator.value = NULL;
			node->data.unary_operator.operand = NULL;
			break;
		case NODE_BINARY_OPERATOR:
			node->data.binary_operator.value = NULL;
			node->data.binary_operator.left = NULL;
			node->data.binary_operator.right = NULL;
			break;
		case NODE_CONSTANT_BOOL:
			node->data.integer.value = 0;
			break;
		case NODE_IDENTIFIER:
		case NODE_TYPE:
		case NODE_CONSTANT_INT:
		case NODE_CONSTANT_FLOAT:
		case NODE_CONSTANT_STRING:
			node->data.string.value = NULL;
			break;
	}
}
