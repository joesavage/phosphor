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

#define DECL_ASTNODE_DATA(node, utype, name) \
struct ASTNode::data::utype &name = node->data.utype

// Tagged unions are 'Ugh.' to work with but have significant enough performance
// gains over dynamic dispatch that I'm just going to run with it.
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
		struct statements {
			MemoryList <ASTNode *> children;
		} statements;
		struct block {
			ASTNode *statements;
			Environment *env;
		} block;
		struct variable_declaration {
			ASTNode *type;
			ASTNode *name;
			ASTNode *init;
			// TODO: Variable modifiers
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
			ASTNode *type;
			MemoryList<ASTNode *> args;
			Environment *env;
			// TODO: Function modifiers
		} function_signature;
		struct function_call {
			ASTNode *name;
			MemoryList<ASTNode *> args;
		} function_call;
	} data;
};

void initialise_node(ASTNode *node, ASTNodeType type);

#endif
