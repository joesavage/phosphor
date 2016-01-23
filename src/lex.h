#ifndef LEX_H
#define LEX_H

#include <stddef.h>
#include <ctype.h>

#include "memoryarena.h"

enum PTokenType {
	TOKEN_UNINITIALIZED,
	TOKEN_RESERVED_PUNCTUATION,
	TOKEN_OPERATOR,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_INT,
	TOKEN_FLOAT,
	TOKEN_STRING
};
static const char *pTokenTypeNames[] = { "TOKEN_UNINITIALIZED",
                                         "TOKEN_RESERVED_PUNCTUATION",
                                         "TOKEN_OPERATOR",
                                         "TOKEN_KEYWORD",
                                         "TOKEN_IDENTIFER",
                                         "TOKEN_INT",
                                         "TOKEN_FLOAT",
                                         "TOKEN_STRING" };

#define MINIMUM_PRECEDENCE 1

struct POperator {
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

	POperator(OperatorType type = BINARY,
		OperatorAssociativity associativity = LEFT_ASSOC,
		unsigned char precedence = 1)
	{
		this->type = type;
		this->associativity = associativity;
		this->precedence = precedence;
	}
};

static inline bool is_radix_prefix(char ch) {
	ch = tolower(ch);
	return (ch == 'x' || ch == 'b' || ch == 'o');
}

struct PToken {
	PTokenType type;
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

	PToken *lex(size_t *count);
private:
	void set_error(const char *format, ...);
	bool peek_whitespace();
	bool peek_comment();
	bool peek_reserved_punctuation();
	void skip_whitespace();
	void skip_comments();
	bool scan_reserved_punctuation(PToken *token);
	bool scan_punctuation(PToken *token);
	bool scan_word(PToken *token);
	bool scan_number(PToken *token);
	bool validate_escape_sequence();
	bool scan_string(PToken *token);
	bool next_token(PToken *token);
};

#endif
