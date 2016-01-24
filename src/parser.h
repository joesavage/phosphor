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
	void set_environment(ASTNode *node,
	                     Environment *parent,
	                     char *current_function = NULL);
	bool eof(int offset = 0);
	bool safe(int offset = 0);
	bool peek_token_type(PTokenType type, int offset = 0);
	bool peek_token(PTokenType type, const char *value, int offset = 0);
	ASTNode *create_node(ASTNodeType type);
	ASTNode *create_node(ASTNodeType type, unsigned int line_no,
	                     unsigned int col_no);
	PToken *scan_token(PTokenType type, const char *value);
	PToken *scan_token_type(PTokenType type);
	bool scan_end_of_line();
	void set_error(const char *format, ...);

	int str_to_size_t(char *str, size_t *result);
	bool peek_constant();
	ASTNode *parse_constant();
	bool peek_identifier();
	ASTNode *parse_identifier();
	bool peek_type(int offset = 0);
	ASTNode *parse_type();
	bool peek_unary_operator();
	ASTNode *parse_unary_operator();
	bool peek_binary_operator();
	ASTNode *parse_binary_operator();

	ASTNode *parse_unary_operators();
	ASTNode *parse_atom();
	ASTNode *parse_expression(unsigned char
	                          minimum_precedence = MINIMUM_PRECEDENCE);
	bool peek_constant_declaration();
	ASTNode *parse_constant_declaration();
	bool peek_variable_declaration();
	ASTNode *parse_variable_declaration();
	ASTNode *parse_block();
	bool peek_function();
	ASTNode *parse_function(ASTNode *function_name);
	ASTNode *parse_if();
	ASTNode *parse_loop();
	ASTNode *parse_return();
	ASTNode *parse_statement();
	ASTNode *parse_statements();
};

#endif
