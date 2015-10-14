#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>

#include "lex.h"
#include "AST.h"
#include "parser.h"
#include "helpers.h"

// TODO: We need to deal with the ambiguity of whether 'parse_' functions
// can result in a half-parsed mess state or not (and, thus, whether we
// can call a particular 'parse_' function to just check whether something
// will parse, or whether that's awaiting a half-parsed disaster).

void Parser::set_error(const char *format, ...) {
	free(error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	error = buffer;
}

void Parser::set_environment(ASTNode *node,
                             Environment *parent,
                             char *current_function)
{
	Environment *env = (Environment *)memory->reserve(sizeof(Environment));
	*env = Environment();
	env->parent = parent;
	env->current_function = current_function ? current_function
	                                         : parent->current_function;

	switch (node->type) {
		case NODE_FUNCTION_SIGNATURE:
			node->toFunctionSignature()->env = env;
			break;
		default:
			node->toBlock()->env = env;
			break;
	}
}

bool Parser::eof(int offset) {
	return (cursor + offset) >= (tokens + token_count);
}

bool Parser::peek_token_type(PTokenType type, int offset) {
	return !eof(offset) && cursor[offset].type == type;
}

bool Parser::peek_token(PTokenType type, const char *value, int offset) {
	return peek_token_type(type, offset) && !strcmp(cursor[offset].value, value);
}

PToken *Parser::scan_token_type(PTokenType type) {
	if (peek_token_type(type))
		return cursor++;
	return NULL;
}

PToken *Parser::scan_token(PTokenType type, const char *value) {
	if (peek_token(type, value))
		return cursor++;
	return NULL;
}

PToken *Parser::scan_end_of_line() {
	// TODO: In future, accept newlines instead of strictly requiring
	// semicolons. Right now though, the parser doesn't know how to handle
	// whitespace (inc. newlines).
	// ALSO: We might run into problems where we didn't expect the parser
	// to care about newlines where we now do. e.g. using newlines in
	// place of semicolons could result in a function with the brace being
	// put on the next line rather than the current line being parsed
	// incorrectly.
	if (peek_token(TOKEN_RESERVED_PUNCTUATION, ";"))
		return cursor++;
	return NULL;
}

ASTNode *Parser::parse_constant() {
	ASTNode *result = NULL;

	// If we want char constants, they should go here too.
	PToken *token = NULL;
	if ((token = scan_token_type(TOKEN_INT))) {
		result = create_node(NODE_CONSTANT_INT);

		int base = 10;
		if (token->value[0] == '0' && is_radix_prefix(token->value[1])) {
			++token->value;
			switch (token->value[0]) {
				case 'x':
					base = 16;
					break;
				case 'o':
					base = 8;
					break;
				case 'b':
					base = 2;
					break;
				default:
					set_error("INTERNAL COMPILER ERROR: bad numeric prefix from lexer");
					return NULL;
			}
			++token->value;
		}

		errno = 0;
		char *endptr = token->value;
		result->toInteger()->value = strtol(token->value, &endptr, base);
		assert(sizeof(result->toInteger()->value) >= 8);
		if (errno == ERANGE) {
			set_error("failed to parse out of range number");
			return NULL;
		} else if (errno != 0
			|| (size_t)(endptr - token->value) < strlen(token->value)) {
			set_error("failed to parse number with base %d", base);
			return NULL;
		}
	} else if ((token = scan_token_type(TOKEN_FLOAT))) {
		result = create_node(NODE_CONSTANT_FLOAT);
		result->toString()->value = token->value;
	} else if ((token = scan_token_type(TOKEN_STRING))) {
		result = create_node(NODE_CONSTANT_STRING);
		result->toString()->value = token->value;
		if (strlen(token->value) <= 2) {
			set_error("empty strings are not allowed");
			return NULL;
		}
	} else if (peek_token(TOKEN_KEYWORD, "true")) {
		++cursor;
		result = create_node(NODE_CONSTANT_BOOL);
		result->toInteger()->value = 1;
	} else if (peek_token(TOKEN_KEYWORD, "false")) {
		++cursor;
		result = create_node(NODE_CONSTANT_BOOL);
		result->toInteger()->value = 0;
	}
	
	return result;
}

ASTNode *Parser::parse_identifier() {
	ASTNode *result = NULL;

	PToken *identifier = scan_token_type(TOKEN_IDENTIFIER);
	if (identifier) {
		result = create_node(NODE_IDENTIFIER);
		result->toString()->value = identifier->value;
	}

	return result;
}

ASTNode *Parser::parse_type() {
	ASTNode *result = NULL;
	if (!peek_token_type(TOKEN_IDENTIFIER))
		return NULL;

	PBaseType *pbasetype = search_for_type(*env, cursor->value);
	if (!pbasetype)
		return NULL;
	result = create_node(NODE_TYPE);
	result->toType()->value = PType(pbasetype);
	scan_token_type(TOKEN_IDENTIFIER);

	// TODO: We need to support parsing of multiple type modifiers. Plus, order matters.
	// e.g. 'int32[5]^[8]' should be an array of eight pointers to arrays of 5 int32s.

	// TODO: 'int[5]^' should be different to 'int^[5]'.
	// TODO: Multi-dimensional arrays.
	if (scan_token(TOKEN_RESERVED_PUNCTUATION, "[")) {
		PToken *array_size_node = scan_token_type(TOKEN_INT);
		if (!array_size_node) {
			set_error("expected array size following '[' in type");
			return NULL;
		}

		// TODO: Review whether using 'atoi' is a good idea or not
		int array_size = atoi(array_size_node->value);
		if (!array_size) {
			set_error("attempted to create array type with invalid size");
			return NULL;
		}

		if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "]")) {
			set_error("expected ']' for array type definition");
			return NULL;
		}

		// We could probably do some more efficient memory allocation here.
		PType *old_ty = (PType *)memory->reserve(sizeof(PType));
		*old_ty = result->toType()->value;
		result->toType()->value = PType(NULL, false, array_size, old_ty);
	}

	if (cursor->value[0] == '^') {
		char *value = scan_token_type(TOKEN_OPERATOR)->value;
		for (size_t i = 0; i < strlen(value); ++i) {
			if (value[i] == '^') {
				// We could probably do some more efficient memory allocation here.
				PType *old_ty = (PType *)memory->reserve(sizeof(PType));
				*old_ty = result->toType()->value;
				result->toType()->value = PType(NULL, true, 0, old_ty);
			} else {
				set_error("unexpected operator in variable declaration");
				return NULL;
			}
		}
	}

	return result;
}

bool Parser::peek_type(int offset) {
	return peek_token_type(TOKEN_IDENTIFIER, offset)
	    && search_for_type(*env, cursor[offset].value) != NULL;
}

bool Parser::peek_unary_operator() {
	return (peek_token_type(TOKEN_OPERATOR)
		  && unary_operators[cursor[0].value])
	    || (peek_token(TOKEN_RESERVED_PUNCTUATION, "(")
	    && peek_type(1));
}

bool Parser::peek_binary_operator() {
	return peek_token_type(TOKEN_OPERATOR)
	    && binary_operators[cursor[0].value];
}

ASTNode *Parser::create_node(ASTNodeType type) {
	ASTNode *result = (ASTNode *)nodes.reserve(sizeof(ASTNode));
	result->initialise(type);
	if (!eof()) {
		result->line_no = cursor->line_no;
		result->col_no = cursor->col_no;
	}
	return result;
}

ASTNode *Parser::parse_unary_operator() {
	ASTNode *result = NULL;

	if (peek_unary_operator()) {
		if (peek_token_type(TOKEN_OPERATOR)) {
			PToken *op = scan_token_type(TOKEN_OPERATOR);
			result = create_node(NODE_UNARY_OPERATOR);
			result->toUnaryOperator()->value = op->value;
		} else { // It must be a cast!
			assert(scan_token(TOKEN_RESERVED_PUNCTUATION, "("));
			ASTNode *type = parse_type();
			assert(type);
			assert(scan_token(TOKEN_RESERVED_PUNCTUATION, ")"));
			result = create_node(NODE_CAST_OPERATOR);
			result->toCastOperator()->type = type->toType()->value;
		}
	}

	return result;
}

ASTNode *Parser::parse_binary_operator() {
	ASTNode *result = NULL;

	if (peek_binary_operator()) {
		PToken *op = scan_token_type(TOKEN_OPERATOR);
		result = create_node(NODE_BINARY_OPERATOR);
		result->toBinaryOperator()->value = op->value;
	}

	return result;
}

ASTNode *Parser::parse_unary_operators() {
	ASTNode *result = NULL;

	ASTNode *last_op;
	ASTNode *op;
	// TODO: Deal with unary precedence?
	// TODO: Deal with prefix vs. postfix?
	while (peek_unary_operator())
	{
		op = parse_unary_operator();

		if (!result) {
			result = last_op = op;
		} else {
			if (last_op->type == NODE_CAST_OPERATOR)
				last_op->toCastOperator()->operand = op;
			else
				last_op->toUnaryOperator()->operand = op;
			last_op = op;
		}
	}

	return result;
}

ASTNode *Parser::parse_atom() {
	// TODO: What about postfix unary operators?
	ASTNode *result = parse_unary_operators();

	// Fix for multiple unary operators. Ideally, this would get returned from
	// 'parse_unary_operators' instead (multiple return values or whatever).
	ASTNode *last_op = result;
	while (last_op && (last_op->type == NODE_UNARY_OPERATOR || last_op->type == NODE_CAST_OPERATOR)) {
		if (last_op->type == NODE_UNARY_OPERATOR) {
			auto operand = last_op->toUnaryOperator()->operand;
			if (!operand)
				break;
			last_op = last_op->toUnaryOperator()->operand;
		} else {
			auto operand = last_op->toCastOperator()->operand;
			if (!operand)
				break;
			last_op = operand;
		}
	}

	ASTNode *&term = result ? (last_op->type == NODE_CAST_OPERATOR ? last_op->toCastOperator()->operand : last_op->toUnaryOperator()->operand) : result;

	if (peek_token_type(TOKEN_IDENTIFIER)) {
		term = parse_identifier();
		assert(term);
		if (scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
			ASTNode *call = create_node(NODE_FUNCTION_CALL);

			if (!peek_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
				do {
					ASTNode *arg = parse_expression();
					if (!arg) {
						set_error("invalid function argument");
						return NULL;
					}
					call->toFunctionCall()->args.add(arg);
				} while (scan_token(TOKEN_RESERVED_PUNCTUATION, ","));
			}

			call->toFunctionCall()->name = term;
			term = call;
			if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
				set_error("expected closing bracket after function call");
				return NULL;
			}
			return result;
		} else {
			return result;
		}
	}

	if ((term = parse_constant()))
		return result;

	if (scan_token(TOKEN_RESERVED_PUNCTUATION, "(")
	    && (term = parse_expression())
	    && scan_token(TOKEN_RESERVED_PUNCTUATION, ")"))
		return result;

	if (result)
		set_error("expected symbol following unary operator");
	return NULL;
}

// Parse expressions via precedence climbing
ASTNode *Parser::parse_expression(unsigned char minimum_precedence)
{
	ASTNode *result = parse_atom();
	if (!result || error)
		return NULL;

	// TODO: Ternary operators need to be handled as a special case here!

	// TODO: This precedence is wrong. We want this to have a higher
	// precedence - it needs to be part of the 'atom'.
	// Parse array accesses
	ASTNode *array_index = NULL;
	if (scan_token(TOKEN_RESERVED_PUNCTUATION, "[")
	    && (array_index = parse_atom())
	    && scan_token(TOKEN_RESERVED_PUNCTUATION, "]"))
	{
		ASTNode *array_access = create_node(NODE_BINARY_OPERATOR);
		array_access->toBinaryOperator()->value = "[]";
		array_access->toBinaryOperator()->left = result;
		array_access->toBinaryOperator()->right = array_index;
		result = array_access;
	}
	if (error)
		return NULL;
		

	POperator operator_properties;
	while (peek_binary_operator())
	{
		POperator operator_properties = binary_operators[cursor[0].value]->value;
		unsigned char precedence = operator_properties.precedence;
		auto associativity = operator_properties.associativity;

		if (precedence < minimum_precedence)
			break;

		ASTNode *op = parse_binary_operator();
		ASTNode *right = NULL;
		assert(op);

		int next_minimum_precedence;
		if (associativity == POperator::RIGHT_ASSOC)
			next_minimum_precedence = precedence;
		else if (associativity == POperator::LEFT_ASSOC)
			next_minimum_precedence = precedence + 1;
		else {
			set_error("expected operator with associativity");
			return NULL;
		}

		right = parse_expression(next_minimum_precedence);
		if (!right) {
			set_error("expected expression after operator");
			return NULL;
		}

		op->toBinaryOperator()->left = result;
		op->toBinaryOperator()->right = right;
		result = op;
	}

	return result;
}

ASTNode *Parser::parse_variable_declaration() {
	ASTNode *result = create_node(NODE_VARIABLE_DECLARATION);

	if (!scan_token(TOKEN_KEYWORD, "let")) {
		ASTNode *type = parse_type();
		// TODO: We need some way to have errors chain together. For
		// example, if 'parse_type' called 'set_error' in this case, then
		// we probably want to display that error along with any additional
		// errors that we want to tack on.
		if (!type) {
			set_error("invalid type in variable declaration");
			return NULL;
		}
		result->toVariableDeclaration()->type = type->toType()->value;
	} else {
		result->toVariableDeclaration()->type = NULL;
	}

	if (peek_token_type(TOKEN_KEYWORD)) {
		set_error("cannot declare variable with reserved keyword name");
		return NULL;
	} else if (!(result->toVariableDeclaration()->name = parse_identifier())) {
		set_error("expected identifier for variable declaration");
		return NULL;
	}

	// Handle assignment after declaration syntax (i.e. 'int32 a = 5')
	if (scan_token(TOKEN_OPERATOR, "=")) {
		result->toVariableDeclaration()->init = parse_expression();
		if (!result->toVariableDeclaration()->init)
			return NULL;
	}

	return result;
}

ASTNode *Parser::parse_block() {
	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "{")) {
		set_error("expected opening brace for block");
		return NULL;
	}
	ASTNode *result = create_node(NODE_BLOCK);
	set_environment(result, env);

	Environment *prev_env = env;
	env = result->toBlock()->env;

	result->toBlock()->statements = parse_statements();
	if (!result->toBlock()->statements || error)
		return NULL;

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "}")) {
		set_error("unexpected symbol in block - '%s'", cursor[0].value);
		return NULL;
	}
	env = prev_env;
	return result;
}

ASTNode *Parser::parse_function() {
	if (!scan_token(TOKEN_KEYWORD, "fn")) {
		set_error("expected 'fn' for function signature");
		return NULL;
	}

	ASTNode *signature = create_node(NODE_FUNCTION_SIGNATURE);

	ASTNode *identifier = parse_identifier();
	if (!identifier) {
		set_error("expected identifier for function signature");
		return NULL;
	}
	signature->toFunctionSignature()->name = identifier;

	// TODO: When we support function overloading, we need to change this line.
	set_environment(signature, env, identifier->toString()->value);

	Environment *prev_env = env;
	env = signature->toFunctionSignature()->env;

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
		set_error("expected opening bracket for function signature");
		return NULL;
	}


	if (!peek_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
		do {
			ASTNode *arg = parse_variable_declaration();
			if (!arg) {
				set_error("invalid variable declaration in function parameter list");
				return NULL;
			}
			signature->toFunctionSignature()->args.add(arg);
		} while (scan_token(TOKEN_RESERVED_PUNCTUATION, ","));
	}

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
		set_error("expected closing bracket in function signature");
		return NULL;
	}

	if (!scan_token(TOKEN_OPERATOR, "->")) {
		set_error("expected '->' return specifier in function signature");
		return NULL;
	}

	// NOTE: We used to check if the type's 'base_type' was NULL here,
	// but I don't think it's necessary as I believe 'parse_type' handles
	// this case.
	ASTNode *return_type = parse_type();
	if (!return_type) {
		set_error("invalid return type in function signature");
		return NULL;
	}
	signature->toFunctionSignature()->type = return_type->toType()->value.base_type;

	if (!peek_token(TOKEN_RESERVED_PUNCTUATION, "{")) {
		env = prev_env;
		return signature;
	} else {
		ASTNode *function = create_node(NODE_FUNCTION);
		function->toFunction()->signature = signature;
		if (!function->toFunction()->signature || error)
			return NULL;
		function->toFunction()->body = parse_block();
		if (!function->toFunction()->body || error)
			return NULL;
		env = prev_env;
		return function;
	}
	
	set_error("expected termination following function signature");
	return NULL;
}

ASTNode *Parser::parse_if() {
	if (!scan_token(TOKEN_KEYWORD, "if")) {
		set_error("expected 'if' keyword for if statement");
		return NULL;
	}

	ASTNode *result = create_node(NODE_IF);
	result->toConditional()->condition = parse_expression();
	if (!result->toConditional()->condition || error)
		return NULL;
	result->toConditional()->then = parse_block();
	if (!result->toConditional()->then || error)
		return NULL;

	if (scan_token(TOKEN_KEYWORD, "else")) {
		if (peek_token(TOKEN_KEYWORD, "if"))
			result->toConditional()->otherwise = parse_if();
		else
			result->toConditional()->otherwise = parse_block();

		if (!result->toConditional()->otherwise || error)
			return NULL;
	}

	return result;
}

ASTNode *Parser::parse_while() {
	if (!scan_token(TOKEN_KEYWORD, "while")) {
		set_error("expected 'while' keyword for while loop");
		return NULL;
	}

	ASTNode *result = create_node(NODE_WHILE_LOOP);
	result->toConditional()->condition = parse_expression();
	if (!result->toConditional()->condition || error)
		return NULL;
	result->toConditional()->then = parse_block();
	if (!result->toConditional()->then || error)
		return NULL;

	if (scan_token(TOKEN_KEYWORD, "else")) {
		result->toConditional()->otherwise = parse_block();
		if (!result->toConditional()->otherwise || error)
			return NULL;
	}

	return result;
}

ASTNode *Parser::parse_return() {
	if (!scan_token(TOKEN_KEYWORD, "return")) {
		set_error("expected 'return' keyword for return signature");
		return NULL;
	}

	ASTNode *result = create_node(NODE_RETURN);
	result->toUnaryOperator()->operand = parse_expression();
	if (!result->toUnaryOperator()->operand)
		set_error("expected expression following 'return' keyword\n");

	return result;
}

// Predictive recursive descent parsing of a single statement
ASTNode *Parser::parse_statement() {
	if (peek_type() || peek_token(TOKEN_KEYWORD, "let")) {
		return parse_variable_declaration();
	} else if (peek_token(TOKEN_KEYWORD, "fn")) {
		return parse_function();
	} else if (peek_token(TOKEN_KEYWORD, "if")) {
		return parse_if();
	} else if (peek_token(TOKEN_KEYWORD, "while")) {
		return parse_while();
	} else if (peek_token(TOKEN_KEYWORD, "return")) {
		return parse_return();
	} else if (peek_token(TOKEN_RESERVED_PUNCTUATION, "{")) {
		return parse_block();
	}
	// ... etc ...

	return parse_expression();
}

ASTNode *Parser::parse_statements() {
	ASTNode *result = NULL;
	ASTNode *statement;

	while (!eof()) {
		if ((!(statement = parse_statement()) || error))
			break;
		
		if (!result)
			result = create_node(NODE_STATEMENTS);
		
		result->toStatements()->children.add(statement);

		if (!scan_end_of_line()) {
			set_error("expected end of line");
			break;
		}
	}

	return result;
}

ASTNode *Parser::parse() {
	if (!env) {
		env = (Environment *)memory->reserve(sizeof(Environment));
		*env = Environment();
	}

	cursor = tokens;

	ASTNode *root = parse_statements();
	if (!eof() && !error)
		set_error("unexpected token encountered\n");

	return root;
}
