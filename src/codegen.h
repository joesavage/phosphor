#ifndef CODEGEN_H
#define CODEGEN_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"

#include "ast.h"
#include "environment.h"

struct CodeGenerator {
	ASTNode *root;
	char *error;

	Environment *env;
	llvm::Module *module;
};

void codegen(CodeGenerator *generator);

#endif
