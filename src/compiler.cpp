#include "helpers.h"
#include "memoryarena.h"
#include "memorylist.hpp"
#include "hashmap.hpp"
#include "lex.h"
#include "parser.h"
#include "codegen.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <new>

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

static void write_module_to_file(Module *module, const char *path) {
	FILE *file = fopen(path, "wb+");
	int fno = fileno(file);
	if (fno == -1 || !file)
		fatal_error("failed to write file '%s'\n", path);
	llvm::raw_fd_ostream ostream(fno, false);
	llvm::raw_ostream *ostreamptr = (llvm::raw_ostream *)&ostream;
	WriteBitcodeToFile(module, *ostreamptr);
}

static void printf_ast(ASTNode *node, const char *prefix, size_t depth = 0) {
	if (node != NULL) {
		for (size_t i = 0; i < depth; ++i)
			fprintf(stderr, "    ");
		if (prefix)
			fprintf(stderr, "%s: ", prefix);
		switch (node->type) {
			case NODE_STATEMENTS:
			{
				auto pdata = *node->toStatements();
				fprintf(stderr, "STATEMENTS\n");
				for (size_t i = 0; i < pdata.children.size(); ++i)
					printf_ast(pdata.children[i], "STMT", depth + 1);
				break;
			}
			case NODE_BLOCK:
			{
				auto pdata = *node->toBlock();
				fprintf(stderr, "BLOCK\n");
				printf_ast(pdata.statements, "STATEMENTS", depth + 1);
				break;
			}
			case NODE_VARIABLE_DECLARATION:
			{
				auto pdata = *node->toVariableDeclaration();
				fprintf(stderr, "VARDECL\n");
				for (size_t i = 0; i <= depth; ++i)
					fprintf(stderr, "    ");
				fprintf(stderr, "TYPE<%s>\n", pdata.type.to_string());
				printf_ast(pdata.name, "NAME", depth + 1);
				printf_ast(pdata.init, "INIT", depth + 1);
				break;
			}
			case NODE_UNARY_OPERATOR:
			{
				auto pdata = *node->toUnaryOperator();
				fprintf(stderr, "UNARY_OP<%s>\n", pdata.value);
				printf_ast(pdata.operand, "OPERAND", depth + 1);
				break;
			}
			case NODE_CAST_OPERATOR:
			{
				auto pdata = *node->toCastOperator();
				fprintf(stderr, "CAST<%s>\n", pdata.type.to_string());
				printf_ast(pdata.operand, "OPERAND", depth + 1);
				break;	
			}
			case NODE_BINARY_OPERATOR:
			{
				auto pdata = *node->toBinaryOperator();
				fprintf(stderr, "BINARY_OP<%s>\n", pdata.value);
				printf_ast(pdata.left, "LEFT", depth + 1);
				printf_ast(pdata.right, "RIGHT", depth + 1);
				break;
			}
			case NODE_CONSTANT_INT:
			case NODE_CONSTANT_BOOL:
			{
				fprintf(stderr, "INT<%zu>\n", node->toInteger()->value);
				break;
			}
			case NODE_TYPE:
			{
				auto pdata = *node->toType();
				fprintf(stderr, "TYPE<%s>\n", pdata.value.to_string());
				break;
			}
			case NODE_IDENTIFIER:
			case NODE_CONSTANT_FLOAT:
			case NODE_CONSTANT_STRING:
			{
				fprintf(stderr, "STRING<%s>\n", node->toString()->value);
				break;
			}
			case NODE_FUNCTION:
			{
				auto pdata = *node->toFunction();
				fprintf(stderr, "FUNCTION\n");
				printf_ast(pdata.signature, "SIG", depth + 1);
				printf_ast(pdata.body, "BODY", depth + 1);
				break;
			}
			case NODE_FUNCTION_SIGNATURE:
			{
				auto pdata = *node->toFunctionSignature();
				fprintf(stderr, "FUNCTION_SIG\n");
				printf_ast(pdata.name, "NAME", depth + 1);
				for (size_t i = 0; i <= depth; ++i)
					fprintf(stderr, "    ");
				fprintf(stderr, "TYPE<%s>\n", pdata.type.to_string());
				for (size_t i = 0; i < pdata.args.size(); ++i)
					printf_ast(pdata.args[i], "ARGS", depth + 1);
				break;
			}
			case NODE_FUNCTION_CALL:
			{
				auto pdata = *node->toFunctionCall();
				fprintf(stderr, "FUNCTION_CALL\n");
				printf_ast(pdata.name, "NAME", depth + 1);
				for (size_t i = 0; i < pdata.args.size(); ++i)
					printf_ast(pdata.args[i], "ARGS", depth + 1);
				break;
			}
			case NODE_RETURN:
			{
				fprintf(stderr, "RETURN\n");
				printf_ast(node->toUnaryOperator()->operand, "EXPR", depth + 1);
				break;
			}
			case NODE_IF:
			{
				fprintf(stderr, "IF\n");
				printf_ast(node->toConditional()->condition, "COND", depth + 1);
				printf_ast(node->toConditional()->then, "THEN", depth + 1);
				printf_ast(node->toConditional()->otherwise, "ELSE", depth + 1);
				break;
			}
			case NODE_FOR_LOOP:
			case NODE_BREAK:
			case NODE_CONTINUE:
			case NODE_ARRAY_INITIALIZATION:
			default:
			{
				fprintf(stderr, "%d\n", node->type);
				break;
			}
		}
	}
}

static void usage(const char *program_name) {
	fatal_error("usage: %s [-O optimisation_level] [-o output_file]\n", program_name);
}

int main(int argc, char *argv[]) {
	Lexer lexer = {};
	Parser parser = {};
	CodeGenerator generator = {};

	MemoryArena transient_memory(2097152);
	lexer.memory = &transient_memory;
	parser.memory = &transient_memory;
	generator.memory = &transient_memory;

	// Parse CLI args
	const char *output_filename = "out.bc";
	int optimisation_level = 0;
	{
		extern char *optarg;
		extern int optind;
		int ch;
		while ((ch = getopt(argc, argv, "h?O:o:")) != EOF) {
			switch(ch) {
				case 'o':
					output_filename = optarg;
					break;
				case 'O':
					optimisation_level = atoi(optarg);
					if (optimisation_level < 0 || optimisation_level > 1)
						fatal_error("illegal optimisation level value\n");
					break;
				case '?':
				case 'h':
				default:
					usage(argv[0]);
			}
		}
		argc -= optind;
		argv += optind;
	}


	HashMap<POperator> &unary_operators = parser.unary_operators;
	unary_operators.size = 16;

	// TODO: Think about precedence properly at some point
	unary_operators.set("+",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 1));
	unary_operators.set("-",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 1));
	unary_operators.set("!",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));
	unary_operators.set("++", POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));
	unary_operators.set("--", POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));
	unary_operators.set("&",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));
	unary_operators.set("*",  POperator(POperator::UNARY, POperator::IMPLICIT_ASSOC, 2));

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
	HashMap<PBaseType *> &type_table = parser.env->type_table;
	HashMap<PVariable> &symbol_table = parser.env->symbol_table;
	HashMap<PFunction> &function_table = parser.env->function_table;
	symbol_table.size = 128;
	type_table.size = 64;
	function_table.size = 32;

	auto type_void = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("void", Type::getVoidTy(getGlobalContext()), 0);
	auto type_bool = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("bool", Type::getInt1Ty(getGlobalContext()), 1);
	auto type_i8 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("i8", Type::getInt8Ty(getGlobalContext()), 8, true, false, true);
	auto type_i16 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("i16", Type::getInt16Ty(getGlobalContext()), 16, true, false, true);
	auto type_i32 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("i32", Type::getInt32Ty(getGlobalContext()), 32, true, false, true);
	auto type_i64 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("i64", Type::getInt64Ty(getGlobalContext()), 64, true, false, true);
	auto type_u8 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("u8", Type::getInt8Ty(getGlobalContext()), 8, true, false, false);
	auto type_u16 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("u16", Type::getInt16Ty(getGlobalContext()), 16, true, false, false);
	auto type_u32 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("u32", Type::getInt32Ty(getGlobalContext()), 32, true, false, false);
	auto type_u64 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("u64", Type::getInt64Ty(getGlobalContext()), 64, true, false, false);
	auto type_f32 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("f32", Type::getFloatTy(getGlobalContext()), 32, true, true, true);
	auto type_f64 = new ((PBaseType *)transient_memory.reserve(sizeof(PBaseType))) PBaseType("f64", Type::getDoubleTy(getGlobalContext()), 64, true, true, true);
	type_table.set(type_void->name, type_void);
	type_table.set(type_bool->name, type_bool);
	type_table.set(type_i8->name, type_i8);
	type_table.set(type_i16->name, type_i16);
	type_table.set(type_i32->name, type_i32);
	type_table.set(type_i64->name, type_i64);
	type_table.set(type_u8->name, type_u8);
	type_table.set(type_u16->name, type_u16);
	type_table.set(type_u32->name, type_u32);
	type_table.set(type_u64->name, type_u64);
	type_table.set(type_f32->name, type_f32);
	type_table.set(type_f64->name, type_f64);
	

	// NOTE: A lot of the code relies on a lot of pointers just about everywhere.
	// Perhaps too much? I feel like maybe more value semantics would result in
	// faster and cleaner code.

	// TODO: Do we want to read the WHOLE text file into memory at once? Hmm...
	lexer.source = read_file("main.ph");

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
	fprintf(stderr, "== TOKENS ==\n");
	for (size_t i = 0; i < parser.token_count; ++i) {
		fprintf(stderr, "%-5zu %-30s\t%s\n", i,
		        pTokenTypeNames[parser.tokens[i].type], parser.tokens[i].value);
	}
	fprintf(stderr, "\n");

	// Parsing
	generator.root = parser.parse();
	free(parser.tokens);
	// TODO: Fancy errors which output the text containing the issue would be good
	// In general, the quality of errors need significantly improving too.
	if (parser.error) {
		// parser.strings.free();
		// parser.nodes.free();
		fatal_error("Parsing failed at line %d, col %d: %s\n",
		            parser.cursor->line_no, parser.cursor->col_no,
		            parser.error);
	}

	// Parse Debug Output
	fprintf(stderr, "== AST ==\n");
	printf_ast(generator.root, "ROOT");
	fprintf(stderr, "\n");

	 // Code Generation
	fprintf(stderr, "== LLVM IR ==\n");
	generator.env = parser.env;
	Module *module = generator.generate(optimisation_level);
	if (generator.error) {
		if (generator.errnode)
			fatal_error("Codegen failed at line %d, col %d: %s\n",
			            generator.errnode->line_no, generator.errnode->col_no,
			            generator.error);
		else
			fatal_error("Codegen error: %s\n", generator.error);
	}

	module->dump();
	write_module_to_file(module, output_filename);

	delete module;
	transient_memory.free();

	return 0;	
}
