#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lex.h"
#include "environment.h"
#include "hashmap.hpp"

struct Parser {
	Token *tokens;
	Token *cursor;
	size_t token_count;
	char *error;

	MemoryArena nodes;
	MemoryArena *memory;
	Environment *env;
	HashMap<Operator> unary_operators;
	HashMap<Operator> binary_operators;
};

// Function signatures
ASTNode *parse_expression(Parser *parser, unsigned char minimum_precedence = MINIMUM_PRECEDENCE);
ASTNode *parse(Parser *parser);

#endif
