#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>

#include "lex.h"
#include "AST.h"
#include "parser.h"
#include "helpers.h"

// TODO: The quality of errors in here needs significantly improving.

void Parser::set_error(const char *format, ...) {
	free(error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	error = buffer;
}

void Parser::set_environment(ASTNode *node, Environment *parent) {
	Environment *env = (Environment *)memory->reserve(sizeof(Environment));
	*env = Environment();
	env->parent = parent;

	switch (node->type) {
		case NODE_FUNCTION_SIGNATURE:
			node->data.function_signature.env = env;
			break;
		default:
			node->data.block.env = env;
			break;
	}
}

bool Parser::eof() {
	return cursor >= (tokens + token_count);
}

bool Parser::peek_token_type(PTokenType type) {
	return !eof() && cursor[0].type == type;
}

bool Parser::peek_token(PTokenType type, const char *value) {
	return peek_token_type(type) && !strcmp(cursor[0].value, value);
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
		result->data.string.value = token->value;
	} else if ((token = scan_token_type(TOKEN_FLOAT))) {
		result = create_node(NODE_CONSTANT_FLOAT);
		result->data.string.value = token->value;
	} else if ((token = scan_token_type(TOKEN_STRING))) {
		result = create_node(NODE_CONSTANT_STRING);
		result->data.string.value = token->value;
	} else if (peek_token(TOKEN_KEYWORD, "true")) {
		++cursor;
		result = create_node(NODE_CONSTANT_BOOL);
		result->data.integer.value = 1;
	} else if (peek_token(TOKEN_KEYWORD, "false")) {
		++cursor;
		result = create_node(NODE_CONSTANT_BOOL);
		result->data.integer.value = 0;
	}
	
	return result;
}

ASTNode *Parser::parse_identifier() {
	ASTNode *result = NULL;

	PToken *identifier = scan_token_type(TOKEN_IDENTIFIER);
	if (identifier) {
		result = create_node(NODE_IDENTIFIER);
		result->data.string.value = identifier->value;
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
		result->data.string.value = cursor->value;
		scan_token_type(TOKEN_IDENTIFIER);
	}

	return result;
}

bool Parser::peek_type() {
	return peek_token_type(TOKEN_IDENTIFIER)
	    && search_for_type(*env, cursor->value) != NULL;
}

bool Parser::peek_unary_operator() {
	return peek_token_type(TOKEN_OPERATOR)
	    && unary_operators[cursor[0].value];
}

bool Parser::peek_binary_operator() {
	return peek_token_type(TOKEN_OPERATOR)
	    && binary_operators[cursor[0].value];
}

ASTNode *Parser::create_node(ASTNodeType type) {
	ASTNode *result = (ASTNode *)nodes.reserve(sizeof(ASTNode));
	initialise_node(result, type);
	if (!eof()) {
		result->line_no = cursor->line_no;
		result->col_no = cursor->col_no;
	}
	return result;
}

ASTNode *Parser::parse_unary_operator() {
	ASTNode *result = NULL;

	if (peek_unary_operator()) {
		PToken *op = scan_token_type(TOKEN_OPERATOR);
		result = create_node(NODE_UNARY_OPERATOR);
		result->data.string.value = op->value;
	}

	return result;
}

ASTNode *Parser::parse_binary_operator() {
	ASTNode *result = NULL;

	if (peek_binary_operator()) {
		PToken *op = scan_token_type(TOKEN_OPERATOR);
		result = create_node(NODE_BINARY_OPERATOR);
		result->data.string.value = op->value;
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
			result = op;
		} else {
			last_op->data.unary_operator.operand = op;
			last_op = op;
		}
	}

	return result;
}

ASTNode *Parser::parse_atom() {
	ASTNode *result = parse_unary_operators();
	ASTNode *&term = result ? result->data.unary_operator.operand : result;

	if ((term = parse_identifier())) {
		if (scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
			ASTNode *call = create_node(NODE_FUNCTION_CALL);

			ASTNode *arg;
			while ((arg = parse_expression())) {
				call->data.function_call.args.add(arg);
				if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ","))
					break;
			}
			// TODO: We actually don't want to ignore all errors here. At all.
			// Only want to ignore some (i.e. failure to parse /anything/) errors.
			error = NULL;

			call->data.function_call.name = term;
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

		op->data.binary_operator.left = result;
		op->data.binary_operator.right = right;
		result = op;
	}

	return result;
}

ASTNode *Parser::parse_variable_declaration() {
	ASTNode *result = create_node(NODE_VARIABLE_DECLARATION);

	result->data.variable_declaration.type = parse_type();

	// TODO: Handle modifiers.

	if (peek_token_type(TOKEN_KEYWORD)) {
		set_error("cannot declare variable with reserved keyword name");
		return NULL;
	} else if (!(result->data.variable_declaration.name = parse_identifier())) {
		set_error("expected identifier for variable declaration");
		return NULL;
	}

	// TODO: Handle assignment immediately after variable declaration (int a = 5).
	// 'result->data.variable_declaration.action' ASTNode

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
	env = result->data.block.env;
	result->data.block.left = parse_statements();
	if (!result->data.block.left || error)
		return NULL;

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "}")) {
		set_error("expected closing brace after block");
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
	set_environment(signature, env);

	ASTNode *identifier = parse_identifier();
	if (!identifier) {
		set_error("expected identifier for function signature");
		return NULL;
	}
	signature->data.function_signature.name = identifier;

	Environment *prev_env = env;
	env = signature->data.function_signature.env;

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, "(")) {
		set_error("expected opening bracket for function signature");
		return NULL;
	}

	ASTNode *arg;
	while ((arg = parse_variable_declaration())) {
		signature->data.function_signature.args.add(arg);
		if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ","))
			break;
	}
	error = NULL; // TODO: We only want to ignore some errors.

	if (!scan_token(TOKEN_RESERVED_PUNCTUATION, ")")) {
		set_error("expected closing bracket in function signature");
		return NULL;
	}

	if (!scan_token(TOKEN_OPERATOR, "->")
	 || !(signature->data.function_signature.type = parse_type()))
	{
		set_error("expected type in function signature");
		return NULL;
	}

	if (scan_token(TOKEN_RESERVED_PUNCTUATION, ";")) {
		env = prev_env;
		return signature;
	} else {
		ASTNode *function = create_node(NODE_FUNCTION);
		function->data.function.signature = signature;
		if (!function->data.function.signature || error)
			return NULL;
		function->data.function.body = parse_block();
		if (!function->data.function.body || error)
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
	result->data.conditional.condition = parse_expression();
	if (!result->data.conditional.condition || error)
		return NULL;
	result->data.conditional.then = parse_block();
	if (!result->data.conditional.then || error)
		return NULL;

	if (scan_token(TOKEN_KEYWORD, "else")) {
		if (peek_token(TOKEN_KEYWORD, "if"))
			result->data.conditional.other = parse_if();
		else
			result->data.conditional.other = parse_block();

		if (!result->data.conditional.other || error)
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
	result->data.conditional.condition = parse_expression();
	if (!result->data.conditional.condition || error)
		return NULL;
	result->data.conditional.then = parse_block();
	if (!result->data.conditional.then || error)
		return NULL;

	if (scan_token(TOKEN_KEYWORD, "else")) {
		result->data.conditional.other = parse_block();
		if (!result->data.conditional.other || error)
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
	result->data.unary_operator.operand = parse_expression();
	if (!result->data.unary_operator.operand || error)
		return NULL;

	return result;
}

// Predictive recursive descent parsing of a single statement
ASTNode *Parser::parse_statement() {
	if (peek_type()) { // TODO: Handle 'let' type inference
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

	ASTNode **current = &result;
	ASTNode *statement;
	while (!eof()) { // TODO: This node structure should be MemoryList of stmts
		if ((!(statement = parse_statement()) || error))
			break;

		if (!(*current))
			*current = create_node(NODE_STATEMENTS);
		(*current)->data.block.left = statement;
		(*current)->data.block.right = NULL;
		current = &(*current)->data.block.right;

		
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
