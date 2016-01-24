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

bool Parser::safe(int offset) {
	return (cursor + offset) < tokens && !eof(offset);
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

bool Parser::scan_end_of_line() {
	if (peek_token(TOKEN_RESERVED_PUNCTUATION, ";")) {
		++cursor;
		return true;
	}

	return eof() || (!safe(-1) && cursor[-1].line_no < cursor[0].line_no);
}

bool Parser::peek_constant() {
	return peek_token_type(TOKEN_INT) || peek_token_type(TOKEN_FLOAT)
	    || peek_token_type(TOKEN_STRING) || peek_token(TOKEN_KEYWORD, "true")
	    || peek_token(TOKEN_KEYWORD, "false");
}

ASTNode *Parser::parse_constant() {
	ASTNode *result = NULL;

	// If we want char constants, they should go here too.
	PToken *token = NULL;
	if ((token = scan_token_type(TOKEN_INT))) {
		result = create_node(NODE_CONSTANT_INT, token->line_no, token->col_no);

		if (str_to_size_t(token->value, &result->toInteger()->value)) {
			if (!error)
				set_error("Failed to parse numeric constant.");
			return NULL;
		}
	} else if ((token = scan_token_type(TOKEN_FLOAT))) {
		result = create_node(NODE_CONSTANT_FLOAT, token->line_no, token->col_no);
		result->toString()->value = token->value;
	} else if ((token = scan_token_type(TOKEN_STRING))) {
		result = create_node(NODE_CONSTANT_STRING, token->line_no, token->col_no);
		result->toString()->value = token->value;
		if (strlen(token->value) == 0) {
			set_error("empty strings are not allowed");
			return NULL;
		}
	} else if (peek_token(TOKEN_KEYWORD, "true")) {
		++cursor;
		result = create_node(NODE_CONSTANT_BOOL, token->line_no, token->col_no);
		result->toInteger()->value = 1;
	} else if (peek_token(TOKEN_KEYWORD, "false")) {
		++cursor;
		result = create_node(NODE_CONSTANT_BOOL, token->line_no, token->col_no);
		result->toInteger()->value = 0;
	}
	
	return result;
}

bool Parser::peek_identifier() {
	return peek_token_type(TOKEN_IDENTIFIER);
}

ASTNode *Parser::parse_identifier() {
	ASTNode *result = NULL;

	PToken *identifier = scan_token_type(TOKEN_IDENTIFIER);
	if (identifier) {
		result = create_node(NODE_IDENTIFIER, identifier->line_no,
		                     identifier->col_no);
		result->toString()->value = identifier->value;
	}

	return result;
}

int Parser::str_to_size_t(char *str, size_t *result) {
	int base = 10;
	if (str[0] == '0' && is_radix_prefix(str[1])) {
		++str;
		switch (str[0]) {
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
				return -1;
		}
		++str;
	}

	errno = 0;
	char *endptr = str;
	*result = strtol(str, &endptr, base);
	if (errno == ERANGE) {
		set_error("failed to parse out of range number");
		return -1;
	} else if (errno != 0
		|| (size_t)(endptr - str) < strlen(str)) {
		set_error("failed to parse number with base %d", base);
		return -1;
	}

	return 0;
}

bool Parser::peek_type(int offset) {
	return peek_token_type(TOKEN_IDENTIFIER, offset)
	    && search_for_type(*env, cursor[offset].value) != NULL;
}

ASTNode *Parser::parse_type() {
	ASTNode *result = NULL;
	if (!peek_token_type(TOKEN_IDENTIFIER))
		return NULL;

	PBaseType *pbasetype = search_for_type(*env, cursor->value);
	if (!pbasetype)
		return NULL;
	result = create_node(NODE_TYPE, cursor->line_no, cursor->col_no);
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

		size_t array_size;
		if (str_to_size_t(array_size_node->value, &array_size)) {
			if (!error)
				set_error("attempted to create array type with invalid size");
			return NULL;
		} else if (!array_size) {
			set_error("cannot create an array with size zero");
			return NULL;
		}

		if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "]")) {
			set_error("expected ']' for array type definition");
			return NULL;
		}

		// We could probably do some more efficient memory allocation here.
		PType *old_ty = (PType *)memory->reserve(sizeof(PType));
		*old_ty = result->toType()->value;
		result->toType()->value = PType(NULL, EMPTY, array_size, old_ty);
	}

	if (cursor->value[0] == '^') {
		char *value = scan_token_type(TOKEN_OPERATOR)->value;
		for (size_t i = 0; i < strlen(value); ++i) {
			if (value[i] == '^') {
				// We could probably do some more efficient memory allocation here.
				PType *old_ty = (PType *)memory->reserve(sizeof(PType));
				*old_ty = result->toType()->value;
				result->toType()->value = PType(NULL, POINTER, 0, old_ty);
			} else {
				set_error("unexpected operator in variable declaration");
				return NULL;
			}
		}
	}

	return result;
}

ASTNode *Parser::create_node(ASTNodeType type, unsigned int line_no,
                             unsigned int col_no) {
	ASTNode *result = (ASTNode *)nodes.reserve(sizeof(ASTNode));
	result->initialise(type);
	result->line_no = line_no;
	result->col_no = col_no;
	return result;
}

bool Parser::peek_unary_operator() {
	return (peek_token_type(TOKEN_OPERATOR)
		  && unary_operators[cursor[0].value])
	    || (peek_token(TOKEN_RESERVED_PUNCTUATION, "(")
	    && peek_type(1));
}

ASTNode *Parser::parse_unary_operator() {
	ASTNode *result = NULL;

	if (peek_unary_operator()) {
		if (peek_token_type(TOKEN_OPERATOR)) {
			PToken *op = scan_token_type(TOKEN_OPERATOR);
			result = create_node(NODE_UNARY_OPERATOR, op->line_no, op->col_no);
			result->toUnaryOperator()->value = op->value;
		} else { // It must be a cast!
			assert(scan_token(TOKEN_RESERVED_PUNCTUATION, "("));
			ASTNode *type = parse_type();
			assert(type);
			assert(scan_token(TOKEN_RESERVED_PUNCTUATION, ")"));
			result = create_node(NODE_CAST_OPERATOR, type->line_no, type->col_no);
			result->toCastOperator()->type = type->toType()->value;
		}
	}

	return result;
}

bool Parser::peek_binary_operator() {
	return peek_token_type(TOKEN_OPERATOR)
	    && binary_operators[cursor[0].value];
}

ASTNode *Parser::parse_binary_operator() {
	ASTNode *result = NULL;

	if (peek_binary_operator()) {
		PToken *op = scan_token_type(TOKEN_OPERATOR);
		result = create_node(NODE_BINARY_OPERATOR, op->line_no, op->col_no);
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
		assert(op);

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
	ASTNode *result = NULL;
	if (peek_unary_operator()) {
		result = parse_unary_operators();
		if (!result) {
			if (!error)
				set_error("failed to parse unary operators");
			return NULL;
		}
	}

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

	ASTNode *&term = result ? (last_op->type == NODE_CAST_OPERATOR
	                            ? last_op->toCastOperator()->operand
	                            : last_op->toUnaryOperator()->operand)
	                        : result;

	if (peek_identifier()) {
		term = parse_identifier();
		assert(term);
		if (scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
			ASTNode *call = create_node(NODE_FUNCTION_CALL, cursor->line_no,
			                            cursor->col_no);

			if (!peek_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
				do {
					ASTNode *arg = parse_expression();
					if (!arg) {
						if (!error)
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

	if (peek_constant()) {
		term = parse_constant();
		if (error)
			return NULL;
		assert(term);
		return result;
	}

	if (scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
		term = parse_expression();
		if (!term) {
			if (!error)
				set_error("expected expression within brackets");
			return NULL;
		}
		if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
			set_error("expected closing bracket");
			return NULL;
		}
		return result;
	}

	if (result)
		set_error("expected symbol following unary operator");
	return NULL;
}

// Parse expressions via precedence climbing
ASTNode *Parser::parse_expression(unsigned char minimum_precedence)
{
	ASTNode *result = parse_atom();
	if (!result || error) {
		if (!error)
			set_error("failed to parse atom in expression");
		return NULL;
	}

	// TODO: Ternary operators need to be handled as a special case here!

	// TODO: This precedence is wrong. We want this to have a higher
	// precedence - it needs to be part of the 'atom'.
	// Parse array accesses
	ASTNode *array_index = NULL;
	if (scan_token(TOKEN_RESERVED_PUNCTUATION, "[")) {
		array_index = parse_atom();
		if (!array_index) {
			if (!error)
				set_error("expected expression for array index");
			return NULL;
		}
		if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "]")) {
			set_error("expected closing bracket for array index");
			return NULL;
		}
		ASTNode *array_access = create_node(NODE_BINARY_OPERATOR,
		                                    array_index->line_no,
		                                    array_index->col_no);
		array_access->toBinaryOperator()->value = "[]";
		array_access->toBinaryOperator()->left = result;
		array_access->toBinaryOperator()->right = array_index;
		result = array_access;
	}
		

	POperator operator_properties;
	while (peek_binary_operator() && cursor[0].line_no == result->line_no)
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
			if (!error)
				set_error("expected expression after operator");
			return NULL;
		}

		op->toBinaryOperator()->left = result;
		op->toBinaryOperator()->right = right;
		result = op;
	}

	return result;
}

bool Parser::peek_constant_declaration() {
	return peek_identifier() && peek_token(TOKEN_OPERATOR, "::", 1);
}

// Merge with 'parse_variable_declaration' when functions are first-class?
ASTNode *Parser::parse_constant_declaration() {
	ASTNode *result = NULL;

	if (peek_token_type(TOKEN_KEYWORD)) {
		set_error("cannot declare constant with reserved keyword name");
		return NULL;
	} else if (!peek_identifier()) {
		set_error("expected identifier for constant declaration");
		return NULL;
	}
	ASTNode *name = parse_identifier();
	assert(name);

	if (!scan_token(TOKEN_OPERATOR, "::")) {
		set_error("expected '::' in constant declaration");
		return NULL;
	}

	if (peek_function()) {
		// TODO: One day, functions will be first-class types and shouldn't need to
		// be treated significantly differently (until codegen).
		if (env->parent != NULL) {
			set_error("functions can only be defined in the global scope");
			return NULL;
		}
		result = parse_function(name);
	} else {
		result = create_node(NODE_VARIABLE_DECLARATION, cursor->line_no,
		                     cursor->col_no);
		result->toVariableDeclaration()->name = name;
		result->toVariableDeclaration()->init = parse_expression();
		result->toVariableDeclaration()->is_constant = true;
		if (!result->toVariableDeclaration()->init) {
			if (!error)
				set_error("invalid expression for constant declaration");
			return NULL;
		}
	}

	return result;
}

bool Parser::peek_variable_declaration() {
	return peek_identifier()
	    && peek_token(TOKEN_OPERATOR, ":", 1)
	    && (peek_type(2) || peek_token(TOKEN_KEYWORD, "auto", 2));
}

ASTNode *Parser::parse_variable_declaration() {
	ASTNode *result = create_node(NODE_VARIABLE_DECLARATION, cursor->line_no,
	                              cursor->col_no);

	if (peek_token_type(TOKEN_KEYWORD)) {
		set_error("cannot declare variable with reserved keyword name");
		return NULL;
	} else if (!peek_identifier()) {
		set_error("expected identifier for variable declaration");
		return NULL;
	}
	result->toVariableDeclaration()->name = parse_identifier();
	assert(result->toVariableDeclaration()->name);

	if (!scan_token(TOKEN_OPERATOR, ":")) {
		set_error("expected ':' in variable declaration");
		return NULL;
	}

	if (!scan_token(TOKEN_KEYWORD, "auto")) {
		ASTNode *type = parse_type();
		// TODO: We need some way to have errors chain together. For
		// example, if 'parse_type' called 'set_error' in this case, then
		// we probably want to display that error along with any additional
		// errors that we want to tack on.
		if (!type) {
			if (!error)
				set_error("invalid type in variable declaration");
			return NULL;
		}
		result->toVariableDeclaration()->type = type->toType()->value;
	} else {
		result->toVariableDeclaration()->type = NULL;
	}

	// Handle assignment after declaration syntax (i.e. 'int32 a = 5')
	if (scan_token(TOKEN_OPERATOR, "=")) {
		result->toVariableDeclaration()->init = parse_expression();
		if (!result->toVariableDeclaration()->init) {
			if (!error)
				set_error("invalid expression for assignment");
			return NULL;
		}
	} else if (result->toVariableDeclaration()->type == NULL) {
		set_error("expected initialisation for type inferred variable");
		return NULL;
	}

	return result;
}

ASTNode *Parser::parse_block() {
	ASTNode *result = create_node(NODE_BLOCK, cursor->line_no, cursor->col_no);
	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "{")) {
		set_error("expected opening brace for block");
		return NULL;
	}
	set_environment(result, env);

	Environment *prev_env = env;
	env = result->toBlock()->env;

	result->toBlock()->statements = parse_statements();
	if (!result->toBlock()->statements || error) {
		if (!error)
			set_error("failed to parse statements in block");
		return NULL;
	}

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "}")) {
		set_error("unexpected symbol in block - '%s'", cursor[0].value);
		return NULL;
	}
	env = prev_env;
	return result;
}

bool Parser::peek_function() {
	if (!peek_token(TOKEN_RESERVED_PUNCTUATION, "("))
		return false;

	int offset = 1;
	int nesting = 1;
	while (nesting > 0) {
		if (peek_token(TOKEN_RESERVED_PUNCTUATION, "(", offset))
			++nesting;
		else if (peek_token(TOKEN_RESERVED_PUNCTUATION, ")", offset))
			--nesting;

		++offset;
		if (offset == INT_MAX || nesting == INT_MAX)
			return false;
	}

	if (nesting < 0 || !peek_token(TOKEN_OPERATOR, "->", offset++))
		return false;

	if (!peek_type(offset))
		return false;

	return true;
}

ASTNode *Parser::parse_function(ASTNode *function_name) {
	ASTNode *signature = create_node(NODE_FUNCTION_SIGNATURE, cursor->line_no,
	                                 cursor->col_no);

	signature->toFunctionSignature()->name = function_name;
	set_environment(signature, env, function_name->toString()->value);

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
				if (!error)
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
	ASTNode *return_type;
	if (!peek_type()) {
		set_error("expected return type in function signature");
		return NULL;
	} else if (!(return_type = parse_type())) {
		if (!error)
			set_error("invalid return type in function signature");
		return NULL;
	}
	signature->toFunctionSignature()->type = return_type->toType()->value;

	if (!peek_token(TOKEN_RESERVED_PUNCTUATION, "{")) {
		env = prev_env;
		return signature;
	} else {
		ASTNode *function = create_node(NODE_FUNCTION, signature->line_no,
		                                signature->col_no);
		function->toFunction()->signature = signature;
		if (!function->toFunction()->signature || error)
			return NULL;
		function->toFunction()->body = parse_block();
		if (!function->toFunction()->body || error) {
			if (!error)
				set_error("failed to parse function block");
			return NULL;
		}
		env = prev_env;
		return function;
	}
	
	set_error("expected termination following function signature");
	return NULL;
}

ASTNode *Parser::parse_if() {
	ASTNode *result = create_node(NODE_IF, cursor->line_no, cursor->col_no);
	if (!scan_token(TOKEN_KEYWORD, "if")) {
		set_error("expected 'if' keyword for if statement");
		return NULL;
	}

	result->toConditional()->condition = parse_expression();
	if (!result->toConditional()->condition || error) {
		if (!error)
			set_error("failed to parse expression in if statement");
		return NULL;
	}
	result->toConditional()->then = parse_block();
	if (!result->toConditional()->then || error) {
		if (!error)
			set_error("failed to parse block for if statement");
		return NULL;
	}

	if (scan_token(TOKEN_KEYWORD, "else")) {
		if (peek_token(TOKEN_KEYWORD, "if"))
			result->toConditional()->otherwise = parse_if();
		else
			result->toConditional()->otherwise = parse_block();

		if (!result->toConditional()->otherwise || error) {
			if (!error)
				set_error("failed to parse 'else' / 'else if' after if statement");
			return NULL;
		}
	}

	return result;
}

ASTNode *Parser::parse_loop() {
	ASTNode *result = create_node(NODE_FOR_LOOP, cursor->line_no, cursor->col_no);
	if (!scan_token(TOKEN_KEYWORD, "for")) {
		set_error("expected 'for' keyword for 'for' loop");
		return NULL;
	}

	result->toConditional()->condition = parse_expression();
	if (!result->toConditional()->condition || error) {
		if (!error)
			set_error("failed to parse expression in for loop");
		return NULL;
	}

	// TODO: Accept traditional for loops with three sections. Comma-separated.

	result->toConditional()->then = parse_block();
	if (!result->toConditional()->then || error) {
		if (!error)
			set_error("failed to parse block in for loop");
		return NULL;
	}

	if (scan_token(TOKEN_KEYWORD, "else")) {
		result->toConditional()->otherwise = parse_block();
		if (!result->toConditional()->otherwise || error) {
			if (!error)
				set_error("failed to parse 'else' block in for loop");
			return NULL;
		}
	}

	return result;
}

ASTNode *Parser::parse_return() {
	ASTNode *result = create_node(NODE_RETURN, cursor->line_no, cursor->col_no);
	if (!scan_token(TOKEN_KEYWORD, "return")) {
		set_error("expected 'return' keyword for return signature");
		return NULL;
	}

	result->toUnaryOperator()->operand = parse_expression();
	if (!result->toUnaryOperator()->operand) {
		if (!error)
			set_error("expected expression following 'return' keyword\n");
		return NULL;
	}

	return result;
}

// Predictive recursive descent parsing of a single statement
ASTNode *Parser::parse_statement() {
	if (peek_variable_declaration()) {
		return parse_variable_declaration();
	} else if (peek_constant_declaration()) {
		return parse_constant_declaration();
	} else if (peek_token(TOKEN_KEYWORD, "if")) {
		return parse_if();
	} else if (peek_token(TOKEN_KEYWORD, "for")) {
		return parse_loop();
	} else if (peek_token(TOKEN_KEYWORD, "return")) {
		return parse_return();
	} else if (peek_token(TOKEN_RESERVED_PUNCTUATION, "{") &&
	           env->parent != NULL) {
		return parse_block();
	}
	// ... etc ...

	if (peek_unary_operator() || peek_identifier() || peek_constant() ||
	    scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
		return parse_expression();
	}

	return NULL;
}

ASTNode *Parser::parse_statements() {
	ASTNode *result = NULL;
	ASTNode *statement;

	while (!eof()) {
		if ((!(statement = parse_statement()) || error))
			break;
		
		if (!result)
			result = create_node(NODE_STATEMENTS, statement->line_no,
			                     statement->col_no);
		
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
