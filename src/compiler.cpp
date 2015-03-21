#include "lex.h"
#include "parser.h"
#include "codegen.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

static char *read_file(char *path) {
	size_t length;
	char *buffer = NULL;

	FILE *file = fopen(path, "rb");
	if (file) {
		fseek(file, 0, SEEK_END);
		length = ftell(file);
		fseek(file, 0, SEEK_SET);
		buffer = (char *)heap_alloc(length + 1);
		fread(buffer, 1, length, file);
		buffer[length] = '\0';
	}
	fclose(file);

	if (buffer == NULL)
		fatal_error("failed to read file '%s'\n", path);

	return buffer;
}

static void printf_ast(ASTNode *node, const char *prefix, size_t depth = 0) {
	if (node != NULL) {
		for (size_t i = 0; i < depth; ++i)
			printf("   ");
		if (prefix)
			printf("%s: ", prefix);
		switch (node->type) {
			case NODE_STATEMENTS:
			{
				DECL_ASTNODE_DATA(node, statements, pdata);
				printf("STATEMENTS\n");
				for (size_t i = 0; i < pdata.children.size(); ++i)
					printf_ast(pdata.children[i], "STMT", depth + 1);
				break;
			}
			case NODE_BLOCK:
			{
				DECL_ASTNODE_DATA(node, block, pdata);
				printf("BLOCK\n");
				printf_ast(pdata.statements, "STATEMENTS", depth + 1);
				break;
			}
			case NODE_VARIABLE_DECLARATION:
			{
				DECL_ASTNODE_DATA(node, variable_declaration, pdata);
				printf("VARDECL\n");
				printf_ast(pdata.type, "TYPE", depth + 1);
				printf_ast(pdata.name, "NAME", depth + 1);
				printf_ast(pdata.action, "ACTION", depth + 1);
				break;
			}
			case NODE_UNARY_OPERATOR:
			{
				DECL_ASTNODE_DATA(node, unary_operator, pdata);
				printf("UNARY_OP<%s>\n", pdata.value);
				printf_ast(pdata.operand, "OPERAND", depth + 1);
				break;
			}
			case NODE_CAST_OPERATOR:
			{
				DECL_ASTNODE_DATA(node, unary_operator, pdata);
				printf("CAST<%s>\n", pdata.value);
				printf_ast(pdata.operand, "OPERAND", depth + 1);
				break;	
			}
			case NODE_BINARY_OPERATOR:
			{
				DECL_ASTNODE_DATA(node, binary_operator, pdata);
				printf("BINARY_OP<%s>\n", pdata.value);
				printf_ast(pdata.left, "LEFT", depth + 1);
				printf_ast(pdata.right, "RIGHT", depth + 1);
				break;
			}
			case NODE_CONSTANT_BOOL:
			{
				printf("INT<%zu>\n", node->data.integer.value);
				break;
			}
			case NODE_TYPE:
			case NODE_IDENTIFIER:
			case NODE_CONSTANT_INT:
			case NODE_CONSTANT_FLOAT:
			case NODE_CONSTANT_STRING:
			{
				printf("STRING<%s>\n", node->data.string.value);
				break;
			}
			case NODE_FUNCTION:
			{
				DECL_ASTNODE_DATA(node, function, pdata);
				printf("FUNCTION\n");
				printf_ast(pdata.signature, "SIG", depth + 1);
				printf_ast(pdata.body, "BODY", depth + 1);
				break;
			}
			case NODE_FUNCTION_SIGNATURE:
			{
				DECL_ASTNODE_DATA(node, function_signature, pdata);
				printf("FUNCTION_SIG\n");
				printf_ast(pdata.name, "NAME", depth + 1);
				printf_ast(pdata.type, "TYPE", depth + 1);
				for (size_t i = 0; i < pdata.args.size(); ++i)
					printf_ast(pdata.args[i], "ARGS", depth + 1);
				break;
			}
			case NODE_FUNCTION_CALL:
			{
				DECL_ASTNODE_DATA(node, function_call, pdata);
				printf("FUNCTION_CALL\n");
				printf_ast(pdata.name, "NAME", depth + 1);
				for (size_t i = 0; i < pdata.args.size(); ++i)
					printf_ast(pdata.args[i], "ARGS", depth + 1);
				break;
			}
			case NODE_RETURN:
			{
				printf("RETURN\n");
				printf_ast(node->data.unary_operator.operand, "EXPR", depth + 1);
				break;
			}
			case NODE_IF:
			{
				printf("IF\n");
				printf_ast(node->data.conditional.condition, "COND", depth + 1);
				printf_ast(node->data.conditional.then, "THEN", depth + 1);
				printf_ast(node->data.conditional.otherwise, "ELSE", depth + 1);
				break;
			}
			case NODE_DO_LOOP:
			case NODE_WHILE_LOOP:
			case NODE_UNTIL_LOOP:
			case NODE_FOR_LOOP:
			case NODE_BREAK:
			case NODE_CONTINUE:
			default:
			{
				printf("%d\n", node->type);
				break;
			}
		}
	}
}

#include "memorylist.hpp"

int main() {
	Lexer lexer = {};
	Parser parser = {};
	CodeGenerator generator = {};

	// TODO: Evaluate this initial size
	MemoryArena transient_memory; //(2097152)
	lexer.memory = &transient_memory;
	parser.memory = &transient_memory;

	HashMap<POperator> &unary_operators = parser.unary_operators;
	unary_operators.size = 16;
	unary_operators.set("+",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 1));
	unary_operators.set("-",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 1));
	unary_operators.set("!",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));
	unary_operators.set("++", POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));
	unary_operators.set("--", POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));

	HashMap<POperator> &binary_operators = parser.binary_operators;
	binary_operators.size = 64;
	binary_operators.set("*",   POperator(POperator::BINARY, POperator::LEFT_ASSOC, 10));
	binary_operators.set("/",   POperator(POperator::BINARY, POperator::LEFT_ASSOC, 10));
	binary_operators.set("+",   POperator(POperator::BINARY, POperator::LEFT_ASSOC,  9));
	binary_operators.set("-",   POperator(POperator::BINARY, POperator::LEFT_ASSOC,  9));
	binary_operators.set("%",   POperator(POperator::BINARY, POperator::LEFT_ASSOC,  9));
	binary_operators.set("<<",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  8));
	binary_operators.set(">>",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  8));
	binary_operators.set("<",   POperator(POperator::BINARY, POperator::LEFT_ASSOC,  7));
	binary_operators.set("<=",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  7));
	binary_operators.set(">",   POperator(POperator::BINARY, POperator::LEFT_ASSOC,  7));
	binary_operators.set(">=",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  7));
	binary_operators.set("==",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  6));
	binary_operators.set("!=",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  6));
	binary_operators.set("&",   POperator(POperator::BINARY, POperator::LEFT_ASSOC,  5));
	binary_operators.set("|",   POperator(POperator::BINARY, POperator::LEFT_ASSOC,  4));
	binary_operators.set("&&",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  3));
	binary_operators.set("||",  POperator(POperator::BINARY, POperator::LEFT_ASSOC,  2));
	binary_operators.set("=",   POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("+=",  POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("-=",  POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("*=",  POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("/=",  POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("%=",  POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("&=",  POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("|=",  POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("&&=", POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("||=", POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set("<<=", POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));
	binary_operators.set(">>=", POperator(POperator::BINARY, POperator::RIGHT_ASSOC, 1));

	parser.env = (Environment *)parser.memory->reserve(sizeof(Environment));
	*parser.env = Environment();
	HashMap<PType> &type_table = parser.env->type_table;
	HashMap<PVariable> &symbol_table = parser.env->symbol_table;
	HashMap<PFunction> &function_table = parser.env->function_table;
	symbol_table.size = 128;
	type_table.size = 64;
	function_table.size = 32;

	type_table.set("void", PType(Type::getVoidTy(getGlobalContext())));
	type_table.set("bool", PType(Type::getInt1Ty(getGlobalContext())));
	type_table.set("int8", PType(Type::getInt8Ty(getGlobalContext()), true, 8));
	type_table.set("int16", PType(Type::getInt16Ty(getGlobalContext()), true, 16));
	type_table.set("int32", PType(Type::getInt32Ty(getGlobalContext()), true, 32));
	type_table.set("int64", PType(Type::getInt64Ty(getGlobalContext()), true, 64));
	type_table.set("uint8", PType(Type::getInt8Ty(getGlobalContext()), false, 8));
	type_table.set("uint16", PType(Type::getInt16Ty(getGlobalContext()), false, 16));
	type_table.set("uint32", PType(Type::getInt32Ty(getGlobalContext()), false, 32));
	type_table.set("uint64", PType(Type::getInt64Ty(getGlobalContext()), false, 64));
	type_table.set("float32", PType(Type::getFloatTy(getGlobalContext()), true, 32));
	type_table.set("float64", PType(Type::getDoubleTy(getGlobalContext()), true, 64));

	// NOTE: A lot of the front-end code relies on a lot of pointers everywhere.
	// Make sure to benchmark the performance of this at some point - the compiler
	// should be really fast! [Maybe we should pass by value more often? esp. in
	// codegen. Benchmark!] Alternatively, if the pointers are just a necessary
	// construct - it's much cleaner to just use C++ member functions than passing
	// the pointer as the first parameter manually in cases to which that applies.

	// TODO: Do we want to read the WHOLE text file into memory at once? Hmm...
	lexer.source = read_file("test.ph");

	// Lexing
	parser.tokens = lexer.lex(&parser.token_count);
	free((void *)lexer.source);
	if (lexer.error) {
		// lexer.strings.free();
		fatal_error("Lexing failed at line %d, col %d: %s\n",
		            lexer.line_no, lexer.cursor - lexer.line_offset + 1,
		            lexer.error);
	}

	// Lex Debug Output
	printf("Tokens: ");
	for (size_t i = 0; i < parser.token_count; ++i)
		printf("%u<%s> ", parser.tokens[i].type, parser.tokens[i].value);
	printf("\n\n");

	// Parsing
	parser.memory = lexer.memory;
	generator.root = parser.parse();
	free(parser.tokens);
	// TODO: Fancy errors which output the text containing the issue would be good
	if (parser.error) {
		// parser.strings.free();
		// parser.nodes.free();
		fatal_error("Parsing failed at line %d, col %d: %s\n",
		            parser.cursor->line_no, parser.cursor->col_no,
		            parser.error);
	}

	// Parse Debug Output
	printf_ast(generator.root, "ROOT");

	 // Code Generation
	printf("\nLLVM IR: \n");
	generator.env = parser.env;
	generator.generate();
	if (generator.error) {
		if (generator.errnode)
			fatal_error("Codegen failed at line %d, col %d: %s\n",
			            generator.errnode->line_no, generator.errnode->col_no,
			            generator.error);
		else
			fatal_error("Codegen error: %s\n", generator.error);
	}

	// parser.memory->free();

	return 0;	
}
