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
	NODE_CAST_OPERATOR,
	NODE_CONSTANT_INT,
	NODE_CONSTANT_FLOAT,
	NODE_CONSTANT_BOOL,
	NODE_CONSTANT_STRING
};

// TODO: Currently we don't do any type checking on this, which is worrying.
#define DECL_ASTNODE_DATA(node, utype, name) \
struct ASTNode::data::utype &name = node->data.utype

// I'm starting to think that using a tagged union was a mistake, but really
// it's the exact data structure I want - it's just that C makes dealing with
// it a massive pain in the arse.
struct ASTNode {
	ASTNodeType type;
	unsigned int line_no;
	unsigned int col_no;
	union data {
		struct real {
			double value;
		} real;
		struct integer {
			size_t value;
		} integer;
		struct string {
			char *value;
		} string;
		struct unary_operator {
			char *value;
			ASTNode *operand;
		} unary_operator;
		struct binary_operator {
			char *value;
			ASTNode *left;
			ASTNode *right;
		} binary_operator;
		struct cast_operator {
			PExType type;
			ASTNode *operand;
		} cast_operator;
		struct statements {
			MemoryList <ASTNode *> children;
		} statements;
		struct block {
			ASTNode *statements;
			Environment *env;
		} block;
		struct variable_declaration {
			PExType type;
			ASTNode *name;
			ASTNode *init;
		} variable_declaration;
		struct conditional {
			ASTNode *condition;
			ASTNode *then;
			ASTNode *otherwise;
		} conditional;
		struct function {
			ASTNode *signature;
			ASTNode *body;
		} function;
		struct function_signature {
			ASTNode *name;
			PExType type;
			MemoryList<ASTNode *> args;
			Environment *env;
			// TODO: Function modifiers
		} function_signature;
		struct function_call {
			ASTNode *name;
			MemoryList<ASTNode *> args;
		} function_call;
		struct type {
			PExType value;
		} type;
	} data;
};

void initialise_node(ASTNode *node, ASTNodeType type);

#endif
