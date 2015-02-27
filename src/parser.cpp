#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>

#include "lex.h"
#include "AST.h"
#include "parser.h"
#include "helpers.h"

// TODO: Move this somewhere sensible.
static ASTNode *parse_statements(Parser *parser);

// TODO: The quality of errors in here needs significantly improving.

static void parser_error(Parser *parser, const char *format, ...) {
	free(parser->error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	parser->error = buffer;
}

static void set_environment(Parser *parser,
                            ASTNode *node,
                            Environment *parent)
{
	Environment *env = (Environment *)parser->memory->reserve(sizeof(Environment));
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

inline static bool eof(Parser *parser) {
	return parser->cursor >= (parser->tokens + parser->token_count);
}

inline static bool peek_token_type(Parser *parser, TokenType type) {
	return !eof(parser) && parser->cursor[0].type == type;
}

inline static bool peek_token(Parser *parser,
                              TokenType type,
                              const char *value)
{
	return peek_token_type(parser, type)
	       && !strcmp(parser->cursor[0].value, value);
}

inline static Token *scan_token_type(Parser *parser, TokenType type) {
	if (peek_token_type(parser, type))
		return parser->cursor++;
	return NULL;
}

inline static Token *scan_token(Parser *parser,
                                TokenType type,
                                const char *value)
{
	if (peek_token(parser, type, value))
		return parser->cursor++;
	return NULL;
}

static ASTNode *parse_constant(Parser *parser) {
	ASTNode *result = NULL;

	// TODO: Refactor/restructure
	Token *token = NULL;
	if ((token = scan_token_type(parser, TOKEN_INT))) {
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_CONSTANT_INT);
		result->data.string.value = token->value;
	} else if ((token = scan_token_type(parser, TOKEN_FLOAT))) {
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_CONSTANT_FLOAT);
		result->data.string.value = token->value;
	} else if ((token = scan_token_type(parser, TOKEN_STRING))) {
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_CONSTANT_STRING);
		result->data.string.value = token->value;
	} else if (peek_token(parser, TOKEN_KEYWORD, "true")) {
		++parser->cursor;
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_CONSTANT_BOOL);
		result->data.integer.value = 1;
	} else if (peek_token(parser, TOKEN_KEYWORD, "false")) {
		++parser->cursor;
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_CONSTANT_BOOL);
		result->data.integer.value = 0;
	}
	
	return result;
}

static ASTNode *parse_identifier(Parser *parser) {
	ASTNode *result = NULL;

	Token *identifier = scan_token_type(parser, TOKEN_IDENTIFIER);
	if (identifier) {
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_IDENTIFIER);
		result->data.string.value = identifier->value;
	}

	return result;
}

static bool peek_type(Parser *parser) {
	return peek_token_type(parser, TOKEN_IDENTIFIER)
	    && search_for_type(*parser->env, parser->cursor->value) != NULL;
}

static ASTNode *parse_type(Parser *parser) {
	ASTNode *result = NULL;
	if (!peek_token_type(parser, TOKEN_IDENTIFIER))
		return NULL;

	PType *ptype = search_for_type(*parser->env, parser->cursor->value);
	if (ptype) {
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_TYPE);
		result->data.string.value = parser->cursor->value;
		scan_token_type(parser, TOKEN_IDENTIFIER);
	}

	return result;
}

static bool peek_unary_operator(Parser *parser) {
	return peek_token_type(parser, TOKEN_OPERATOR)
	    && parser->unary_operators[parser->cursor[0].value];
}

static bool peek_binary_operator(Parser *parser) {
	return peek_token_type(parser, TOKEN_OPERATOR)
	    && parser->binary_operators[parser->cursor[0].value];
}

static ASTNode *parse_unary_operator(Parser *parser) {
	ASTNode *result = NULL;

	if (peek_unary_operator(parser)) {
		Token *op = scan_token_type(parser, TOKEN_OPERATOR);
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_UNARY_OPERATOR);
		result->data.string.value = op->value;
	}

	return result;
}

static ASTNode *parse_binary_operator(Parser *parser) {
	ASTNode *result = NULL;

	if (peek_binary_operator(parser)) {
		Token *op = scan_token_type(parser, TOKEN_OPERATOR);
		result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(result, NODE_BINARY_OPERATOR);
		result->data.string.value = op->value;
	}

	return result;
}

ASTNode *parse_unary_operators(Parser *parser) {
	ASTNode *result = NULL;

	ASTNode *last_op;
	ASTNode *op;
	// TODO: Deal with unary precedence?
	// TODO: Deal with prefix vs. postfix?
	while (peek_unary_operator(parser))
	{
		op = parse_unary_operator(parser);
		if (!result) {
			result = op;
		} else {
			last_op->data.unary_operator.operand = op;
			last_op = op;
		}
	}

	return result;
}

static ASTNode *parse_atom(Parser *parser) {
	ASTNode *result = parse_unary_operators(parser);
	ASTNode *&term = result ? result->data.unary_operator.operand : result;

	if ((term = parse_identifier(parser))) {
		if (scan_token(parser, TOKEN_RESERVED_PUNCTUATION, "(")) {
			ASTNode *call = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
			initialise_node(call, NODE_FUNCTION_CALL);

			ASTNode *arg;
			while ((arg = parse_expression(parser))) {
				call->data.function_call.args.add(arg);
				if (!scan_token(parser, TOKEN_RESERVED_PUNCTUATION, ","))
					break;
			}
			parser->error = NULL;

			call->data.function_call.name = term;
			term = call;
			if (!scan_token(parser, TOKEN_RESERVED_PUNCTUATION, ")")) {
				parser_error(parser, "expected closing bracket after function call");
				return NULL;
			}
			return result;
		} else {
			return result;
		}
	}

	if ((term = parse_constant(parser)))
		return result;
	if (scan_token(parser, TOKEN_RESERVED_PUNCTUATION, "(")
	    && (term = parse_expression(parser))
	    && scan_token(parser, TOKEN_RESERVED_PUNCTUATION, ")"))
		return result;
	return NULL;
}

// Parse expressions via precedence climbing
ASTNode *parse_expression(Parser *parser, unsigned char minimum_precedence) {
	ASTNode *result = parse_atom(parser); // Variable or constant or bracketed expression
	if (!result)
		return NULL;

	// TODO: Ternary operators and square bracket operators need to be handled specially here.

	// If no operators follow, the expression is just a term.
	// If multiple same-precedence operators follow, chain them 'left-recursively' in the tree.
	Operator operator_properties;
	while (peek_binary_operator(parser))
	{
		Operator operator_properties = parser->binary_operators[parser->cursor[0].value]->value;
		unsigned char precedence = operator_properties.precedence;
		Operator::OperatorAssociativity associativity = operator_properties.associativity;

		if (precedence < minimum_precedence)
			break;

		ASTNode *op = parse_binary_operator(parser);
		ASTNode *right = NULL;

		int next_minimum_precedence;
		if (associativity == Operator::RIGHT_ASSOC)
			next_minimum_precedence = precedence;
		else if (associativity == Operator::LEFT_ASSOC)
			next_minimum_precedence = precedence + 1;
		else {
			parser_error(parser, "expected operator with associativity");
			return NULL;
		}

		right = parse_expression(parser, next_minimum_precedence);

		if (!right) {
			parser_error(parser, "expected expression after operator");
			return NULL;
		}

		op->data.binary_operator.left = result;
		op->data.binary_operator.right = right;
		result = op;
	}

	return result;
}

static ASTNode *parse_variable_declaration(Parser *parser) {
	ASTNode *result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
	initialise_node(result, NODE_VARIABLE_DECLARATION);

	result->data.variable_declaration.type = parse_type(parser);

	// TODO: Handle modifiers.

	if (peek_token_type(parser, TOKEN_KEYWORD)) {
		parser_error(parser, "cannot declare variable with reserved keyword name");
		return NULL;
	} else if (!(result->data.variable_declaration.name = parse_identifier(parser))) {
		// TODO: Better error handling for if a reserved keyword follows?
		parser_error(parser, "expected identifier for variable declaration");
		return NULL;
	}

	// TODO: Handle assignment immediately after variable declaration (int a = 5).
	// 'result->data.variable_declaration.action' ASTNode

	return result;
}

static ASTNode *parse_block(Parser *parser) {
	if (!scan_token(parser, TOKEN_RESERVED_PUNCTUATION, "{")) {
		parser_error(parser, "expected opening brace for block");
		return NULL;
	}
	ASTNode *result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
	initialise_node(result, NODE_BLOCK);
	set_environment(parser, result, parser->env);

	Environment *prev_env = parser->env;
	parser->env = result->data.block.env;
	result->data.block.left = parse_statements(parser);
	if (!scan_token(parser, TOKEN_RESERVED_PUNCTUATION, "}")) {
		parser_error(parser, "expected closing brace after block");
		return NULL;
	}
	parser->env = prev_env;
	return result;
}

static ASTNode *parse_function(Parser *parser) {
	if (!scan_token(parser, TOKEN_KEYWORD, "fn")) {
		parser_error(parser, "expected 'fn' for function signature");
		return NULL;
	}

	ASTNode *signature = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
	initialise_node(signature, NODE_FUNCTION_SIGNATURE);
	set_environment(parser, signature, parser->env);

	ASTNode *identifier = parse_identifier(parser);
	if (!identifier) {
		parser_error(parser, "expected identifier for function signature");
		return NULL;
	}
	signature->data.function_signature.name = identifier;

	Environment *prev_env = parser->env;
	parser->env = signature->data.function_signature.env;

	if (!scan_token(parser, TOKEN_RESERVED_PUNCTUATION, "(")) {
		parser_error(parser, "expected opening bracket for function signature");
		return NULL;
	}

	ASTNode *arg;
	while ((arg = parse_variable_declaration(parser))) {
		signature->data.function_signature.args.add(arg);
		if (!scan_token(parser, TOKEN_RESERVED_PUNCTUATION, ","))
			break;
	}
	parser->error = NULL;

	if (!scan_token(parser, TOKEN_RESERVED_PUNCTUATION, ")")) {
		parser_error(parser, "expected closing bracket in function signature");
		return NULL;
	}

	if (!scan_token(parser, TOKEN_OPERATOR, "->")
	 || !(signature->data.function_signature.type = parse_type(parser)))
	{
		parser_error(parser, "expected type in function signature");
		return NULL;
	}

	if (scan_token(parser, TOKEN_RESERVED_PUNCTUATION, ";")) {
		parser->env = prev_env;
		return signature;
	} else {
		ASTNode *function = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
		initialise_node(function, NODE_FUNCTION);
		function->data.function.signature = signature;
		function->data.function.body = parse_block(parser);
		parser->env = prev_env;
		return function;
	}
	
	parser_error(parser, "expected termination following function signature");
	return NULL;
}

static ASTNode *parse_if(Parser *parser) {
	if (!scan_token(parser, TOKEN_KEYWORD, "if")) {
		parser_error(parser, "expected 'if' keyword for if statement");
		return NULL;
	}

	ASTNode *result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
	initialise_node(result, NODE_IF);
	result->data.conditional.condition = parse_expression(parser);
	result->data.conditional.then = parse_block(parser);

	if (scan_token(parser, TOKEN_KEYWORD, "else")) {
		if (peek_token(parser, TOKEN_KEYWORD, "if"))
			result->data.conditional.other = parse_if(parser);
		else
			result->data.conditional.other = parse_block(parser);
	}

	return result;
}

static ASTNode *parse_while(Parser *parser) {
	if (!scan_token(parser, TOKEN_KEYWORD, "while")) {
		parser_error(parser, "expected 'while' keyword for while loop");
		return NULL;
	}

	ASTNode *result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
	initialise_node(result, NODE_WHILE_LOOP);
	result->data.conditional.condition = parse_expression(parser);
	result->data.conditional.then = parse_block(parser);

	if (scan_token(parser, TOKEN_KEYWORD, "else"))
		result->data.conditional.other = parse_block(parser);

	return result;
}

static ASTNode *parse_return(Parser *parser) {
	if (!scan_token(parser, TOKEN_KEYWORD, "return")) {
		parser_error(parser, "expected 'return' keyword for return signature");
		return NULL;
	}

	ASTNode *result = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
	initialise_node(result, NODE_RETURN);
	result->data.unary_operator.operand = parse_expression(parser);

	return result;
}

// Predictive recursive descent parsing of a single statement
static ASTNode *parse_statement(Parser *parser) {
	// TODO: Try to parse a statement (by checking identifier token values etc.)

	if (peek_type(parser)) { // TODO: Handle 'let'
		return parse_variable_declaration(parser);
	} else if (peek_token(parser, TOKEN_KEYWORD, "fn")) {
		return parse_function(parser);
	} else if (peek_token(parser, TOKEN_KEYWORD, "if")) {
		return parse_if(parser);
	} else if (peek_token(parser, TOKEN_KEYWORD, "while")) {
		return parse_while(parser);
	} else if (peek_token(parser, TOKEN_KEYWORD, "return")) {
		return parse_return(parser);
	} else if (peek_token(parser, TOKEN_RESERVED_PUNCTUATION, "{")) {
		return parse_block(parser);
	}
	// ... etc ...

	return parse_expression(parser);
}

static ASTNode *parse_statements(Parser *parser) {
	ASTNode *result = NULL;

	ASTNode **current = &result;
	ASTNode *statement;
	while (!eof(parser)) {
		if ((!(statement = parse_statement(parser)) || parser->error))
			break;

		if (!(*current)) {
			*current = (ASTNode *)parser->nodes.reserve(sizeof(ASTNode));
			initialise_node(*current, NODE_STATEMENTS);

			// PLAN: When we different environments associated with nodes we simply
			// save the current environment, set the current environment to a new env,
			// parse statements under this new env, and then restore the old env.
			(*current)->data.block.env = parser->env;
		}
		(*current)->data.block.left = statement;
		(*current)->data.block.right = NULL;
		current = &(*current)->data.block.right;

		
		scan_token(parser, TOKEN_RESERVED_PUNCTUATION, ";");
	}

	return result;
}

ASTNode *parse(Parser *parser)
{
	if (!parser->env) {
		parser->env = (Environment *)parser->memory->reserve(sizeof(Environment));
		*parser->env = Environment();
	}

	parser->cursor = parser->tokens;

	ASTNode *root = parse_statements(parser);
	if (!eof(parser) && !parser->error)
		parser_error(parser, "unexpected token encountered\n");

	return root;
}
