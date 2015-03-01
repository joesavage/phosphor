#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lex.h"
#include "environment.h"
#include "hashmap.hpp"

struct Parser {
	PToken *tokens;
	PToken *cursor;
	size_t token_count;
	char *error;
	MemoryArena nodes;
	MemoryArena *memory;
	Environment *env;
	HashMap<POperator> unary_operators;
	HashMap<POperator> binary_operators;

	ASTNode *parse();
private:
	void set_environment(ASTNode *node, Environment *parent);
	bool eof();
	bool peek_token_type(PTokenType type);
	bool peek_token(PTokenType type, const char *value);
	bool peek_type();
	bool peek_unary_operator();
	bool peek_binary_operator();
	ASTNode *create_node(ASTNodeType type);
	PToken *scan_token(PTokenType type, const char *value);
	PToken *scan_token_type(PTokenType type);
	void set_error(const char *format, ...);

	ASTNode *parse_constant();
	ASTNode *parse_identifier();
	ASTNode *parse_type();
	ASTNode *parse_unary_operator();
	ASTNode *parse_binary_operator();
	ASTNode *parse_unary_operators();
	ASTNode *parse_atom();
	ASTNode *parse_expression(unsigned char
	                          minimum_precedence = MINIMUM_PRECEDENCE);
	ASTNode *parse_variable_declaration();
	ASTNode *parse_block();
	ASTNode *parse_function();
	ASTNode *parse_if();
	ASTNode *parse_while();
	ASTNode *parse_return();
	ASTNode *parse_statement();
	ASTNode *parse_statements();
};

#endif
