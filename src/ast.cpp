#include <stddef.h>

#include "ast.h"

void ASTNode::initialise(ASTNodeType type) {
	this->type = type;

	switch(type) {
		case NODE_STATEMENTS:
			toStatements()->children = MemoryList<ASTNode *>();
			break;
		case NODE_BLOCK:
			toBlock()->statements = NULL;
			toBlock()->env = NULL;
			break;
		case NODE_CONSTANT_DECLARATION:
		case NODE_VARIABLE_DECLARATION:
			toVariableDeclaration()->type = PType();
			toVariableDeclaration()->name = NULL;
			toVariableDeclaration()->init = NULL;
			break;
		case NODE_IF:
		case NODE_DO_LOOP:
		case NODE_WHILE_LOOP:
		case NODE_UNTIL_LOOP:
			toConditional()->condition = NULL;
			toConditional()->then = NULL;
			toConditional()->otherwise = NULL;
			break;
		case NODE_FUNCTION:
			toFunction()->signature = NULL;
			toFunction()->body = NULL;
			break;
		case NODE_FUNCTION_SIGNATURE:
			toFunctionSignature()->name = NULL;
			toFunctionSignature()->type = PType();
			toFunctionSignature()->args = MemoryList<ASTNode *>();
			toFunctionSignature()->env = NULL;
			break;
		case NODE_FUNCTION_CALL:
			toFunctionCall()->name = NULL;
			toFunctionCall()->args = MemoryList<ASTNode *>();
			break;
		case NODE_FOR_LOOP:
		case NODE_BREAK:
		case NODE_CONTINUE:
		case NODE_RETURN:
			break;
		case NODE_CAST_OPERATOR:
			toCastOperator()->type = PType();
			toCastOperator()->operand = NULL;
			break;
		case NODE_UNARY_OPERATOR:
			toUnaryOperator()->value = NULL;
			toUnaryOperator()->operand = NULL;
			break;
		case NODE_BINARY_OPERATOR:
			toBinaryOperator()->value = NULL;
			toBinaryOperator()->left = NULL;
			toBinaryOperator()->right = NULL;
			break;
		case NODE_CONSTANT_INT:
		case NODE_CONSTANT_BOOL:
			toInteger()->value = 0;
			break;
		case NODE_TYPE:
			toType()->value = PType();
			break;
		case NODE_IDENTIFIER:
		case NODE_CONSTANT_FLOAT:
		case NODE_CONSTANT_STRING:
			toString()->value = NULL;
			break;
		case NODE_VOID:
			break;
	}
}

struct ASTNode::data::real *ASTNode::toReal() {
	// assert
	return &this->data.real;
}
struct ASTNode::data::integer *ASTNode::toInteger() {
	assert(type == NODE_CONSTANT_INT || type == NODE_CONSTANT_BOOL);
	return &this->data.integer;
}
struct ASTNode::data::string *ASTNode::toString() {
	assert(type == NODE_CONSTANT_STRING || type == NODE_IDENTIFIER ||
	       type == NODE_CONSTANT_FLOAT);
	return &this->data.string;
}
struct ASTNode::data::unary_operator *ASTNode::toUnaryOperator() {
	assert(type == NODE_UNARY_OPERATOR || type == NODE_RETURN);
	return &this->data.unary_operator;
}
struct ASTNode::data::binary_operator *ASTNode::toBinaryOperator() {
	assert(type == NODE_BINARY_OPERATOR);
	return &this->data.binary_operator;
}
struct ASTNode::data::cast_operator *ASTNode::toCastOperator() {
	assert(type == NODE_CAST_OPERATOR);
	return &this->data.cast_operator;
}
struct ASTNode::data::statements *ASTNode::toStatements() {
	assert(type == NODE_STATEMENTS);
	return &this->data.statements;
}
struct ASTNode::data::block *ASTNode::toBlock() {
	assert(type == NODE_BLOCK);
	return &this->data.block;
}
struct ASTNode::data::variable_declaration *ASTNode::toVariableDeclaration() {
	assert(type == NODE_VARIABLE_DECLARATION
	    || type == NODE_CONSTANT_DECLARATION);
	return &this->data.variable_declaration;
}
struct ASTNode::data::conditional *ASTNode::toConditional() {
	assert(type == NODE_IF || type == NODE_WHILE_LOOP);
	return &this->data.conditional;
}
struct ASTNode::data::function *ASTNode::toFunction() {
	assert(type == NODE_FUNCTION);
	return &this->data.function;
}
struct ASTNode::data::function_signature *ASTNode::toFunctionSignature() {
	assert(type == NODE_FUNCTION_SIGNATURE);
	return &this->data.function_signature;
}
struct ASTNode::data::function_call *ASTNode::toFunctionCall() {
	assert(type == NODE_FUNCTION_CALL);
	return &this->data.function_call;
}
struct ASTNode::data::type *ASTNode::toType() {
	assert(type == NODE_TYPE);
	return &this->data.type;
}
