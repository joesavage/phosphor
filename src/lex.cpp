#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stddef.h>
#include <ctype.h>
#include <string.h>

#include "lex.h"
#include "helpers.h"

// NOTE: We should probably do the decoding of values (e.g. numbers) here

// NOTE: The errors in here need improving.

static int isoctal(int ch) { return '0' <= ch && ch <= '7'; }
static int isbinary(int ch) { return '0' <= ch && ch <= '1'; }

static bool is_radix_prefix(char ch) {
	ch = tolower(ch);
	return (ch == 'x' || ch == 'b' || ch == 'o');
}

static void lexer_error(Lexer *lexer, const char *format, ...) {
	free(lexer->error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	lexer->error = buffer;
}

static bool peek_whitespace(Lexer *lexer) {
	return isspace(lexer->cursor[0]);
}

static bool peek_comment(Lexer *lexer) {
	return (lexer->cursor[0] == '/' && lexer->cursor[1] == '/')
	    || (lexer->cursor[0] == '/' && lexer->cursor[1] == '*');
}

static bool peek_reserved_punctuation(Lexer *lexer) {
	return lexer->cursor[0] == '(' || lexer->cursor[0] == ')'
	    || lexer->cursor[0] == '{' || lexer->cursor[0] == '}'
	    || lexer->cursor[0] == '[' || lexer->cursor[0] == ']'
	    || lexer->cursor[0] == ';' || lexer->cursor[0] == '"'
	    || lexer->cursor[0] == ',' || lexer->cursor[0] == '\'';
}

static void skip_whitespace(Lexer *lexer) {
	while (peek_whitespace(lexer)) { 
		if (lexer->cursor[0] == '\n') {
			if (lexer->cursor[1] == '\r')
				++lexer->cursor;

			lexer->line_no++;
			lexer->line_offset = ++lexer->cursor;
		} else if (lexer->cursor[0] == '\r') {
			if (lexer->cursor[1] == '\n')
				++lexer->cursor;

			lexer->line_no++;
			lexer->line_offset = ++lexer->cursor;
		} else {
			++lexer->cursor;
		}
	}
}

static void skip_comments(Lexer *lexer) { // TODO: Nested comments
	while (peek_comment(lexer)) {
		if (lexer->cursor[0] == '/' && lexer->cursor[1] == '*') {
			lexer->cursor += 2;
			if (!lexer->cursor[0]) {
				lexer_error(lexer, "unterminated block comment");
				return;
			}

			while(!(lexer->cursor[0] == '*' && lexer->cursor[1] == '/')) {
				if (lexer->cursor[0] == 0 || lexer->cursor[1] == 0) {
					lexer_error(lexer, "unterminated block comment");
					return;
				}

				if (peek_whitespace(lexer))
					skip_whitespace(lexer);
				else
					lexer->cursor++;
			}

			lexer->cursor += 2;
		} else {
			while (lexer->cursor[0] != 0
			    && lexer->cursor[0] != '\n'
			    && lexer->cursor[0] != '\r') {
			    	lexer->cursor++;
			}

			if (lexer->cursor[0])
				skip_whitespace(lexer);
		}
	}
}

static bool scan_reserved_punctuation(Lexer *lexer, Token *token) {
	token->type = TOKEN_RESERVED_PUNCTUATION;
	token->value = lexer->memory->strndup(lexer->cursor++, 1);

	return true;
}

static bool scan_punctuation(Lexer *lexer, Token *token) {
	const char *begin = lexer->cursor;

	while (ispunct(lexer->cursor[0]) && !peek_reserved_punctuation(lexer))
		lexer->cursor++;

	token->type = TOKEN_OPERATOR;
	token->value = lexer->memory->strndup(begin, lexer->cursor - begin);

	return true;
}

static bool scan_word(Lexer *lexer, Token *token) {
	const char *begin = lexer->cursor;

	while (isalnum(lexer->cursor[0]) || lexer->cursor[0] == '_')
		lexer->cursor++;

	bool is_keyword = false;
	const char *keywords[] = {
		"if", "else", "true", "false", "break", "continue",
		"do", "while", "until", "for",
		"fn", "return", "let"
	};
	for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); ++i) {
		unsigned long length = (unsigned long)(lexer->cursor - begin);
		bool match = (length == strlen(keywords[i]));
		match = match && !strncmp(begin, keywords[i], length);
		is_keyword = is_keyword || match;
	}

	token->type = is_keyword ? TOKEN_KEYWORD : TOKEN_IDENTIFIER;
	token->value = lexer->memory->strndup(begin, lexer->cursor - begin);

	return true;
}

static bool scan_number(Lexer *lexer, Token *token) {
	const char *begin = lexer->cursor;

	// Non-decimal radix prefixes
	if (begin[0] == '0' && is_radix_prefix(begin[1])) {
		lexer->cursor++;
		int (*validate)(int) = NULL;
		switch (tolower(*lexer->cursor++)) {
			case 'x':
				validate = isxdigit;
				break;
			case 'o':
				validate = isoctal;
				break;
			case 'b':
				validate = isbinary;
				break;
			default:
				unreachable_code_path();
				break;
		}

		while (validate(lexer->cursor[0]))
			lexer->cursor++;

		if (lexer->cursor - begin <= 2) {
			lexer_error(lexer, "expected digits after radix prefix");
			return false;
		}

		token->type = TOKEN_INT;
		token->value = lexer->memory->strndup(begin, lexer->cursor - begin);

		return true;
	} else {
		bool isFloat = false;
		while (isdigit(lexer->cursor[0]))
			lexer->cursor++;

		if (lexer->cursor[0] == '.') {
			isFloat = true;
			lexer->cursor++;
			while (isdigit(lexer->cursor[0]))
				lexer->cursor++;
		}

		if (tolower(lexer->cursor[0]) == 'e') {
			isFloat = true;
			lexer->cursor++;

			if (lexer->cursor[0] == '+' || lexer->cursor[0] == '-')
				lexer->cursor++;

			if (!isdigit(lexer->cursor[0])) {
				lexer_error(lexer, "missing floating point exponent");
				return false;
			}

			while (isdigit(lexer->cursor[0]))
				lexer->cursor++;
		}

		token->type = isFloat ? TOKEN_FLOAT : TOKEN_INT;
		token->value = lexer->memory->strndup(begin, lexer->cursor - begin);
		
		return true;
	}

	return false;
}

static bool validate_escape_sequence(Lexer *lexer) {
	switch(*++lexer->cursor) {
		case '\\':
		case '/':
		case '\'':
		case '"':
		case 'a':
		case 'b':
		case 'f':
		case 'n':
		case 'r':
		case 't':
			lexer->cursor++;
			return true;
		case 'x': {
			lexer->cursor++;
			if (isxdigit(lexer->cursor[0]) && isxdigit(lexer->cursor[1])) {
				lexer->cursor += 2;
				return true;
			} else {
				lexer_error(lexer, "invalid hex escape sequence: '\\x%c%c\n",
				              lexer->cursor[0], lexer->cursor[1]);
				return false;
			}
		}
	}

	lexer_error(lexer, "invalid escape sequence: '\\%c'", lexer->cursor[0]);
	return false;
}

static bool scan_string(Lexer *lexer, Token *token) {
	const char *begin = lexer->cursor++;

	while (lexer->cursor[0] != '"') {
		if (!lexer->cursor[0]) {
			lexer_error(lexer, "unterminated string");
			return false;
		} else if (lexer->cursor[0] == '\n' || lexer->cursor[0] == '\r') {
			lexer_error(lexer, "unexpected newline in string");
			return false;
		} else if (lexer->cursor[0] == '\\') {
			if (!validate_escape_sequence(lexer))
				return false;
		} else {
			lexer->cursor++;
		}
	}
	lexer->cursor++;

	token->type = TOKEN_STRING;
	token->value = lexer->memory->strndup(begin, lexer->cursor - begin);

	return true;
}

static bool next_token(Lexer *lexer, Token *token) {
	// Skip whitespace and comments
	while (peek_whitespace(lexer) || peek_comment(lexer)) {
		skip_whitespace(lexer);
		skip_comments(lexer);
	}

	token->line_no = lexer->line_no;
	token->col_no = lexer->cursor - lexer->line_offset + 1;
	token->offset = lexer->cursor - lexer->source;

	if (lexer->cursor[0] == '"')
		return scan_string(lexer, token);
	if (peek_reserved_punctuation(lexer))
		return scan_reserved_punctuation(lexer, token);
	if (ispunct(lexer->cursor[0]))
		return scan_punctuation(lexer, token);
	if (lexer->cursor[0] == '_' || isalpha(lexer->cursor[0]))
		return scan_word(lexer, token);
	if (isdigit(lexer->cursor[0]))
		return scan_number(lexer, token);

	if (lexer->cursor[0] == 0) {
		lexer->eof = true;
		return 0;
	}

	lexer_error(lexer, "unexpected character '%c'\n", lexer->cursor[0]);
	return 0;
}

// TODO: Introduce some form of read-buffering so large files
// don't have to occupy memory all at once [switch between two page read?]
// NOTE: Although, we have to store the whole program in memory for the
// token stream anyway so it only prevents the double memory load (which is
// good, but not quite what I had hoped)
Token *lex(Lexer *lexer, size_t *count) {
	// Lexer initialization
	lexer->cursor = lexer->source;
	lexer->line_offset = lexer->source;
	lexer->line_no = 1;
	lexer->eof = false;
	lexer->error = NULL;

	size_t token_chunk_size = 256;
	size_t token_count = 0;
	Token *tokens = (Token *)heap_alloc(token_chunk_size * sizeof(tokens[0]));

	while (next_token(lexer, &tokens[token_count])) {
		++token_count;
		if (token_count + 1 == token_chunk_size) {
			token_chunk_size *= 2;

			Token *oldTokens = tokens;
			tokens = (Token *)heap_alloc(token_chunk_size * sizeof(tokens[0]));
			memcpy(tokens, oldTokens, token_count * sizeof(tokens[0]));
			free(oldTokens);
		}
	}

	// If we haven't matched a token but haven't finished lexing
	// all the input, an error must have occured.
	if (!lexer->eof) {
		lexer->memory->free();
		free(tokens);
		*count = 0;
		return NULL;
	}

	*count = token_count;
	return tokens;
}
