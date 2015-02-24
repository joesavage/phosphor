#ifndef AST_H
#define AST_H

#include "environment.h"
#include "memorylist.hpp"

#define MINIMUM_OPERATOR_PRECISION 1

enum ASTNodeType {
	NODE_STATEMENTS,
	NODE_VARIABLE_DECLARATION,
	NODE_IF,
	NODE_DO_LOOP,
	NODE_WHILE_LOOP,
	NODE_UNTIL_LOOP,
	NODE_FOR_LOOP,
	NODE_BREAK,
	NODE_CONTINUE,
	NODE_BLOCK,
	NODE_FUNCTION,
	NODE_FUNCTION_SIGNATURE,
	NODE_FUNCTION_CALL,
	NODE_RETURN,
	NODE_IDENTIFIER,
	NODE_TYPE,
	NODE_UNARY_OPERATOR,
	NODE_BINARY_OPERATOR,
	NODE_CONSTANT_INT,
	NODE_CONSTANT_FLOAT,
	NODE_CONSTANT_STRING
};

// TOTHINK: Union memory bugs can be tough to detect. Plus, having to re-specify
// the 'type' of an ASTNode everywhere ('node->type.whatever') is annoying.
// Inheritance would be nice. I may have made a poor architecture decision.
struct ASTNode {
	ASTNodeType type;
	union {
		struct {
			double value;
		} real; // NOTE: Currently unused
		struct {
			size_t value;
		} integer; // NOTE: Currently unused
		struct {
			char *value;
		} string;
		struct {
			char *value;
			ASTNode *operand;
		} unary_operator;
		struct {
			char *value;
			ASTNode *left;
			ASTNode *right;
		} binary_operator;
		struct {
			ASTNode *left;
			ASTNode *right;
			Environment *env;
		} block;
		struct {
			ASTNode *type;
			ASTNode *name;
			ASTNode *action;
		} variable_declaration;
		struct {
			ASTNode *condition;
			ASTNode *then;
			ASTNode *other;
		} conditional;
		struct {
			ASTNode *signature;
			ASTNode *body;
		} function;
		struct {
			ASTNode *name; // TODO:/TOTHINK: Unnecessary indirection
			ASTNode *type;
			MemoryList<ASTNode *> args;
			Environment *env;
		} function_signature;
		struct {
			ASTNode *name;
			MemoryList<ASTNode *> args;
		} function_call;
	};
	
	ASTNode(ASTNodeType type);
};

#endif
