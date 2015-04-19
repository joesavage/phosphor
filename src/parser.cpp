#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>

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

ASTNode *Parser::parse_constant() {
	ASTNode *result = NULL;

	PToken *token = NULL;
	if ((token = scan_token_type(TOKEN_INT))) {
		result = create_node(NODE_CONSTANT_INT);
		result->toString()->value = token->value;
	} else if ((token = scan_token_type(TOKEN_FLOAT))) {
		result = create_node(NODE_CONSTANT_FLOAT);
		result->toString()->value = token->value;
	} else if ((token = scan_token_type(TOKEN_STRING))) {
		result = create_node(NODE_CONSTANT_STRING);
		result->toString()->value = token->value;
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

	PType *ptype = search_for_type(*env, cursor->value);
	if (ptype) {
		result = create_node(NODE_TYPE);
		result->toType()->value = PExType(cursor->value);
		scan_token_type(TOKEN_IDENTIFIER);
	}

	// TODO: Handle other modifiers (though, should they always have to be after
	// the typename like this?)
	if (cursor->value[0] == '^') {
		char *value = scan_token_type(TOKEN_OPERATOR)->value;
		for (size_t i = 0; i < strlen(value); ++i) {
			if (value[i] == '^') {
				result->toType()->value.pointer_level += 1;
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

	// TODO: Add dereference and address-of operators.

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
	ASTNode *result = parse_unary_operators();
	ASTNode *&term = result ? (result->type == NODE_CAST_OPERATOR ? result->toCastOperator()->operand : result->toUnaryOperator()->operand) : result;

	if ((term = parse_identifier())) {
		if (scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
			ASTNode *call = create_node(NODE_FUNCTION_CALL);

			ASTNode *arg;
			// TODO: 'func(arg1, arg2,)' parses without errors at current.
			while ((arg = parse_expression())) {
				call->toFunctionCall()->args.add(arg);
				if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ","))
					break;
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
ASTNode *Parser::parse_expression(unsigned char minimum_precedence) {
	ASTNode *result = parse_atom();
	if (!result || error)
		return NULL;

	// TODO: Ternary operators and square bracket operators need to be handled
	// especially here!

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

	if (!scan_token(TOKEN_KEYWORD, "let"))
		result->toVariableDeclaration()->type = parse_type()->toType()->value;

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

	ASTNode *arg;
	while ((peek_type()) && (arg = parse_variable_declaration())) {
		signature->toFunctionSignature()->args.add(arg);
		if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ","))
			break;
	}

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
		set_error("expected closing bracket in function signature");
		return NULL;
	}

	if (!scan_token(TOKEN_OPERATOR, "->")
	 || !(signature->toFunctionSignature()->type
	      = parse_type()->toType()->value).is_set())
	{
		set_error("expected type in function signature");
		return NULL;
	}

	if (scan_token(TOKEN_RESERVED_PUNCTUATION, ";")) {
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
		error = NULL;
	else if (error)
		return NULL;

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
		scan_token(TOKEN_RESERVED_PUNCTUATION, ";");
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
