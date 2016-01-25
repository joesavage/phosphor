#ifndef AST_H
#define AST_H

#include "environment.h"
#include "memorylist.hpp"

enum ASTNodeType {
	NODE_VOID,
	NODE_STATEMENTS,
	NODE_VARIABLE_DECLARATION,
	NODE_IF,
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

class ASTNode {
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
			PType type;
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
			PType type;
			ASTNode *name;
			ASTNode *init;
			bool is_constant;
		} variable_declaration;
		struct conditional {
			ASTNode *condition;
			ASTNode *then;
			ASTNode *otherwise;
		} conditional;
		struct for_loop {
			ASTNode *initialization;
			ASTNode *condition;
			ASTNode *update;
			ASTNode *then;
			ASTNode *otherwise;
		} for_loop;
		struct function {
			ASTNode *signature;
			ASTNode *body;
		} function;
		struct function_signature {
			ASTNode *name;
			PType type;
			MemoryList<ASTNode *> args;
			Environment *env;
			// TODO: Function modifiers
		} function_signature;
		struct function_call {
			ASTNode *name;
			MemoryList<ASTNode *> args;
		} function_call;
		struct type {
			PType value;
		} type;
	} data;

public:
	ASTNodeType type;
	unsigned int line_no;
	unsigned int col_no;

	ASTNode();
	void initialise(ASTNodeType type = NODE_VOID);
	struct data::real *toReal();
	struct data::integer *toInteger();
	struct data::string *toString();
	struct data::unary_operator *toUnaryOperator();
	struct data::binary_operator *toBinaryOperator();
	struct data::cast_operator *toCastOperator();
	struct data::statements *toStatements();
	struct data::block *toBlock();
	struct data::variable_declaration *toVariableDeclaration();
	struct data::conditional *toConditional();
	struct data::for_loop *toForLoop();
	struct data::function *toFunction();
	struct data::function_signature *toFunctionSignature();
	struct data::function_call *toFunctionCall();
	struct data::type *toType();
};

#endif
