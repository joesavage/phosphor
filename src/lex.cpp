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

static int isoctal(int ch) {
	return '0' <= ch && ch <= '7';
}

static int isbinary(int ch) {
	return '0' <= ch && ch <= '1';
}

static bool is_radix_prefix(char ch) {
	ch = tolower(ch);
	return (ch == 'x' || ch == 'b' || ch == 'o');
}

void Lexer::set_error(const char *format, ...) {
	free(error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	error = buffer;
}

bool Lexer::peek_whitespace() {
	return isspace(cursor[0]);
}

bool Lexer::peek_comment() {
	return (cursor[0] == '/' && cursor[1] == '/')
	    || (cursor[0] == '/' && cursor[1] == '*');
}

bool Lexer::peek_reserved_punctuation() {
	return cursor[0] == '(' || cursor[0] == ')'
	    || cursor[0] == '{' || cursor[0] == '}'
	    || cursor[0] == '[' || cursor[0] == ']'
	    || cursor[0] == ';' || cursor[0] == '"'
	    || cursor[0] == ',' || cursor[0] == '\'';
}

void Lexer::skip_whitespace() {
	while (peek_whitespace()) { 
		if (cursor[0] == '\n') {
			if (cursor[1] == '\r')
				++cursor;

			line_no++;
			line_offset = ++cursor;
		} else if (cursor[0] == '\r') {
			if (cursor[1] == '\n')
				++cursor;

			line_no++;
			line_offset = ++cursor;
		} else {
			++cursor;
		}
	}
}

void Lexer::skip_comments() { // TODO: Nested comments
	while (peek_comment()) {
		if (cursor[0] == '/' && cursor[1] == '*') {
			cursor += 2;
			if (!cursor[0]) {
				set_error("unterminated block comment");
				return;
			}

			while(!(cursor[0] == '*' && cursor[1] == '/')) {
				if (cursor[0] == 0 || cursor[1] == 0) {
					set_error("unterminated block comment");
					return;
				}

				if (peek_whitespace())
					skip_whitespace();
				else
					cursor++;
			}

			cursor += 2;
		} else {
			while (cursor[0] != 0
			    && cursor[0] != '\n'
			    && cursor[0] != '\r') {
			    	cursor++;
			}

			if (cursor[0])
				skip_whitespace();
		}
	}
}

bool Lexer::scan_reserved_punctuation(PToken *token) {
	token->type = TOKEN_RESERVED_PUNCTUATION;
	token->value = memory->strndup(cursor++, 1);

	return true;
}

bool Lexer::scan_punctuation(PToken *token) {
	const char *begin = cursor;

	while (ispunct(cursor[0]) && !peek_reserved_punctuation())
		cursor++;

	token->type = TOKEN_OPERATOR;
	token->value = memory->strndup(begin, cursor - begin);

	return true;
}

bool Lexer::scan_word(PToken *token) {
	const char *begin = cursor;

	while (isalnum(cursor[0]) || cursor[0] == '_')
		cursor++;

	bool is_keyword = false;
	const char *keywords[] = {
		"if", "else", "true", "false", "break", "continue",
		"do", "while", "until", "for",
		"fn", "return", "let"
	};
	for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); ++i) {
		unsigned long length = (unsigned long)(cursor - begin);
		bool match = (length == strlen(keywords[i]));
		match = match && !strncmp(begin, keywords[i], length);
		is_keyword = is_keyword || match;
	}

	token->type = is_keyword ? TOKEN_KEYWORD : TOKEN_IDENTIFIER;
	token->value = memory->strndup(begin, cursor - begin);

	return true;
}

bool Lexer::scan_number(PToken *token) {
	const char *begin = cursor;

	// Non-decimal radix prefixes
	if (begin[0] == '0' && is_radix_prefix(begin[1])) {
		cursor++;
		int (*validate)(int) = NULL;
		switch (tolower(*cursor++)) {
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

		while (validate(cursor[0]))
			cursor++;

		if (cursor - begin <= 2) {
			set_error("expected digits after radix prefix");
			return false;
		}

		token->type = TOKEN_INT;
		token->value = memory->strndup(begin, cursor - begin);

		return true;
	} else {
		bool is_float = false;
		while (isdigit(cursor[0]))
			cursor++;

		if (cursor[0] == '.') {
			is_float = true;
			cursor++;
			while (isdigit(cursor[0]))
				cursor++;
		}

		if (tolower(cursor[0]) == 'e') {
			is_float = true;
			cursor++;

			if (cursor[0] == '+' || cursor[0] == '-')
				cursor++;

			if (!isdigit(cursor[0])) {
				set_error("missing floating point exponent");
				return false;
			}

			while (isdigit(cursor[0]))
				cursor++;
		}

		token->type = is_float ? TOKEN_FLOAT : TOKEN_INT;
		token->value = memory->strndup(begin, cursor - begin);
		
		return true;
	}

	return false;
}

bool Lexer::validate_escape_sequence() {
	switch(*++cursor) {
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
			cursor++;
			return true;
		case 'x': {
			cursor++;
			if (isxdigit(cursor[0]) && isxdigit(cursor[1])) {
				cursor += 2;
				return true;
			} else {
				set_error("invalid hex escape sequence: '\\x%c%c\n",
				          cursor[0], cursor[1]);
				return false;
			}
		}
	}

	set_error("invalid escape sequence: '\\%c'", cursor[0]);
	return false;
}

bool Lexer::scan_string(PToken *token) {
	const char *begin = cursor++;

	while (cursor[0] != '"') {
		if (!cursor[0]) {
			set_error("unterminated string");
			return false;
		} else if (cursor[0] == '\n' || cursor[0] == '\r') {
			set_error("unexpected newline in string");
			return false;
		} else if (cursor[0] == '\\') {
			if (!validate_escape_sequence())
				return false;
		} else {
			cursor++;
		}
	}
	cursor++;

	token->type = TOKEN_STRING;
	token->value = memory->strndup(begin, cursor - begin);

	return true;
}

bool Lexer::next_token(PToken *token) {
	// Skip whitespace and comments
	while (peek_whitespace() || peek_comment()) {
		skip_whitespace();
		skip_comments();
	}

	if (error)
		return false;

	token->line_no = line_no;
	token->col_no = cursor - line_offset + 1;
	token->offset = cursor - source;

	if (cursor[0] == '"')
		return scan_string(token);
	if (peek_reserved_punctuation())
		return scan_reserved_punctuation(token);
	if (ispunct(cursor[0]))
		return scan_punctuation(token);
	if (cursor[0] == '_' || isalpha(cursor[0]))
		return scan_word(token);
	if (isdigit(cursor[0]))
		return scan_number(token);

	if (cursor[0] == 0) {
		eof = true;
		return false;
	}

	set_error("unexpected character '%c'\n", cursor[0]);
	return false;
}

// TODO: Introduce some form of read-buffering so large files
// don't have to occupy memory all at once [switch between two page read?]
PToken *Lexer::lex(size_t *count) {
	// Lexer initialization
	cursor = source;
	line_offset = source;
	line_no = 1;
	eof = false;
	error = NULL;

	size_t token_chunk_size = 256;
	size_t token_count = 0;
	PToken *tokens = (PToken *)heap_alloc(token_chunk_size * sizeof(tokens[0]));

	while (next_token(&tokens[token_count])) {
		++token_count;
		if (token_count + 1 == token_chunk_size) {
			token_chunk_size *= 2;

			PToken *oldPTokens = tokens;
			tokens = (PToken *)heap_alloc(token_chunk_size * sizeof(tokens[0]));
			memcpy(tokens, oldPTokens, token_count * sizeof(tokens[0]));
			free(oldPTokens);
		}
	}

	// If we haven't matched a token but haven't finished lexing
	// all the input, an error must have occured.
	if (!eof) {
		memory->free();
		free(tokens);
		*count = 0;
		return NULL;
	}

	*count = token_count;
	return tokens;
}
