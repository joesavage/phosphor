#ifndef LEX_H
#define LEX_H

#include <stddef.h>

#include "memoryarena.h"

enum TokenType {
	TOKEN_UNINITIALIZED,
	TOKEN_RESERVED_PUNCTUATION,
	TOKEN_OPERATOR,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_INT,
	TOKEN_FLOAT,
	TOKEN_STRING
};

#define MINIMUM_PRECEDENCE 1

struct Operator {
	char key[128];
	enum OperatorType {
		BINARY,
		UNARY
	} type;
	enum OperatorAssociativity {
		IMPLICIT_ASSOC,
		LEFT_ASSOC,
		RIGHT_ASSOC
	} associativity;
	unsigned char precedence;

	Operator(OperatorType type = BINARY,
		OperatorAssociativity associativity = LEFT_ASSOC,
		unsigned char precedence = 1)
	{
		this->type = type;
		this->associativity = associativity;
		this->precedence = precedence;
	}
};

struct Token {
	TokenType type;
	size_t offset;
	char *value;

	unsigned int line_no;
	unsigned int col_no;
};

struct Lexer {
	const char *source;
	const char *cursor;
	const char *line_offset;
	unsigned int line_no;
	bool eof;
	char *error;

	MemoryArena *memory;
};

// Function signatures
Token *lex(Lexer *lexer, size_t *count);

#endif
