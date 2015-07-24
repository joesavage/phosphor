#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/CallingConv.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Transforms/Scalar.h"

#include "codegen.h"
#include "hashmap.hpp"
#include "environment.h"
#include "helpers.h"

using namespace llvm;

// TODO: Clean up this whole file. It needs to be a lot tighter than current.

// TODO: The quality of errors in here needs SIGNIFICANTLY improving.

// TODO: At some point, we need to focus on quality of the LLVM IR generated.
//       [COMPARE TO: $ clang -S -emit-llvm foo.c]

void CodeGenerator::set_error(ASTNode *node, const char *format, ...) {
	free(error);

	char *buffer = (char *)heap_alloc(512);
	va_list arglist;
	va_start(arglist, format);
	vsnprintf(buffer, 512, format, arglist);
	va_end(arglist);

	error = buffer;
	errnode = node;
}

PBaseType *CodeGenerator::lookup_base_type(const char *name) {
	return search_for_type(*env, name);
}

PFunction *CodeGenerator::lookup_function(const char *name) {
	return search_for_function(*env, name);
}

PVariable *CodeGenerator::lookup_symbol(const char *name) {
	return search_for_symbol(*env, name);
}

PValue CodeGenerator::get_boolean_value(bool value) {
	PValue result;
	result.type = PType(lookup_base_type("bool"));
	result.llvmval = ConstantInt::get(getGlobalContext(),
	                                  APInt(result.type.getBaseType()->numbits,
	                                  value));
	return result;
}

AllocaInst *CodeGenerator::create_entry_block_alloca(char *name, Type *type) {
	PFunction *function = lookup_function(env->current_function);
	if (!function) {
		fatal_error("invalid 'current' function for alloca creation\n");
		return NULL;
	}

	BasicBlock *entry_bb = &function->llvmval->getEntryBlock();
	IRBuilder<> entry_builder(entry_bb, entry_bb->begin());
	return entry_builder.CreateAlloca(type, 0, name);
}

bool CodeGenerator::implicit_type_convert(PValue *source,
                                          PType dest_type)
{
	if (source->type == dest_type)
		return true;

	PBaseType *source_base = source->type.getBaseType();
	PBaseType *dest_base = dest_type.getBaseType();
	Type *source_llvm = source->type.getLLVMType();
	Type *dest_llvm = dest_type.getLLVMType();

	// For now, pointers and arrays don't do any implicit type conversion.
	if (source->type.is_pointer || dest_type.is_pointer
	 || source->type.array_size || dest_type.array_size)
		return false;

	if (source_base->is_numeric && dest_base->is_numeric) {
		// TODO: Consider whether integer to float implicit conversion is a good
		// idea or not.
		// TODO: This big unwieldy condition can probably be simplified down!
		if ((((source_base->is_signed == dest_base->is_signed
		 && (!source_base->is_float))
		 || (source_base->is_float && dest_base->is_float))
		 && ((source_base->numbits <= dest_base->numbits)
		 || (!source_base->is_float && dest_base->is_float)))

		 // NOTE: unsigned->signed conversions are allowed if there is no possible
		 // truncation. implicit signed->unsigned conversions are disallowed.
		 // TODO: Should we be more relaxed with this check? I'm unsure.
		 || ((!source_base->is_signed && dest_base->is_signed)
		 && (dest_base->numbits - 1 >= source_base->numbits))) {
			// We could easily create the cast instructions manually rather than
			// relying on 'getCastOpcode' here if required (Trunc, ZExt/SExt, etc.).
			if (CastInst::isCastable(source_llvm, dest_llvm)) {
				source->type = dest_type;
				auto cast_opcode = CastInst::getCastOpcode(source->llvmval,
				                                           source_base->is_signed,
				                                           dest_llvm,
				                                           dest_base->is_signed);
				source->llvmval = builder->CreateCast(cast_opcode,
				                                      source->llvmval,
				                                      dest_llvm);

				assert(source->type.getLLVMType() == dest_type.getLLVMType());
				assert(source->llvmval->getType() == dest_type.getLLVMType());
				return true;
			}
		}
	}

	return false;
}

bool CodeGenerator::explicit_type_convert(PValue *source,
                                          PType dest_type)
{
	if (source->type == dest_type)
		return true;

	PBaseType *source_base = source->type.getBaseType();
	PBaseType *dest_base = dest_type.getBaseType();
	Type *source_llvm = source->type.getLLVMType();
	Type *dest_llvm = dest_type.getLLVMType();

	// For now, we'll just do any casts that LLVM thinks we can do.
	// Think about this more in future (incl. bitcasts, etc.).
	if (CastInst::isCastable(source_llvm, dest_llvm)) {
		source->type = dest_type;
		auto cast_opcode = CastInst::getCastOpcode(source->llvmval,
		                                           source_base->is_signed,
		                                           dest_llvm,
		                                           dest_base->is_signed);
		source->llvmval = builder->CreateCast(cast_opcode,
		                                      source->llvmval,
		                                      dest_llvm);

		assert(source->type.getLLVMType() == dest_type.getLLVMType());
		assert(source->llvmval->getType() == dest_type.getLLVMType());
		return true;
	}

	return false;
}

// NOTE: This generates mis-matching types and values - the caller should beware
// that the type returned is for the loaded value, not the value returned (as,
// in the context of this member function, the loaded value is the thing of
// importance).
PVariable CodeGenerator::generate_lvalue(ASTNode *node) {
	PVariable result;

	switch (node->type) {
		case NODE_VARIABLE_DECLARATION:
		{
			auto pnode = *node->toVariableDeclaration();
			char *variable_name = pnode.name->toString()->value;
			PType variable_type;
			if (pnode.type.base_type || pnode.type.indirect_type)
				variable_type = pnode.type;
			assert(variable_type.base_type || variable_type.indirect_type || pnode.init);
			if (env->symbol_table.exists(variable_name)) {
				set_error(pnode.name, "variable naming conflict");
				break;
			}

			PValue value;
			if (pnode.init) {
				value = generate_rvalue(pnode.init);
				if (error) {
					break;
				} else if (variable_type.base_type) {
					if (!implicit_type_convert(&value, variable_type)) {
						set_error(pnode.init, "type mismatch in variable initialization - "
						          "expected '%s', got '%s'", variable_type.to_string(),
					            value.type.to_string());
						break;
					}
				} else { // Type inference
					// TODO: Maybe we want to do something with the type here.
					// If we're using an 8-bit int literal, for example, we might want
					// to actually declare the variable as 32-bits in size.
					result.type = value.type;
				}
			}
			if (variable_type.base_type || variable_type.indirect_type)
				result.type = variable_type;

			// alloca at the entry block so that the mem2reg pass can hit
			Type *variable_llvm_type = result.type.getLLVMType();
			result.llvmval = create_entry_block_alloca(variable_name,
			                                           variable_llvm_type);
			env->symbol_table.set(variable_name, result);

			if (pnode.init)
				builder->CreateStore(value.llvmval, result.llvmval);
			break;
		}
		case NODE_IDENTIFIER:
		{
			auto pnode = *node->toString();
			PVariable *value = lookup_symbol(pnode.value);
			if (!value) {
				set_error(node, "failed to find symbol '%s'", pnode.value);
				break;
			}

			result.type = value->type;
			result.llvmval = value->llvmval;
			break;
		}
		case NODE_UNARY_OPERATOR:
		{
			auto pnode = *node->toUnaryOperator();

			if (!strcmp(pnode.value, "*")) {
				PValue value = generate_rvalue(pnode.operand);
				if (error)
					break;

				if (!value.type.is_pointer) {
					set_error(pnode.operand, "cannot dereference non-pointer operand");
					break;
				}
				result.type = *value.type.indirect_type;
				result.llvmval = (AllocaInst *)value.llvmval;
				break;
			}

			set_error(node, "unsupported unary operator '%s' for generating lvalue", pnode.value);
			break;
		}
		case NODE_BINARY_OPERATOR:
		{
			auto pnode = *node->toBinaryOperator();

			if (!strcmp(pnode.value, "[]")) {
				PVariable left = generate_lvalue(pnode.left);
				if (error)
					break;

				PValue right = generate_rvalue(pnode.right);
				if (error)
					break;

				// TODO: The errors for this will probably be terrible right now.

				if (left.type.array_size) {
					std::vector<Value *> indices;
					indices.push_back(builder->getInt32(0));
					indices.push_back(right.llvmval);
					result.type = *left.type.indirect_type;
					result.llvmval = (AllocaInst *)builder->CreateGEP(left.llvmval, indices);
					break;
				}
				break;
			}

			set_error(node, "unsupported binary operator '%s' for generating lvalue", pnode.value);
			break;
		}
		// TODO: NODE_BINARY_OPERATOR FOR '[]'
		default:
			set_error(node, "failed to generate variable for ASTNode");
			break;
	}

	return result;
}

PValue CodeGenerator::generate_rvalue(ASTNode *node) {
	PValue result;

	switch (node->type) {
		case NODE_BINARY_OPERATOR:
		{
			auto pnode = *node->toBinaryOperator();

			// TODO: Handle other operators (+=, etc.)
			if (!strcmp(pnode.value, "=")) {
				PVariable left = generate_lvalue(pnode.left);
				if (error)
					break;
				if (!isa<AllocaInst>(left.llvmval)) {
					set_error(pnode.left, "value on left of assignment is not an lvalue");
					break;
				}

				PValue value = generate_rvalue(pnode.right);
				if (error)
					break;

				if (!implicit_type_convert(&value, left.type)) {
					set_error(pnode.right, "type mismatch in assignment - expected '%s', "
					          "got '%s'", left.type.to_string(),
					          value.type.to_string());
					break;
				}

				builder->CreateStore(value.llvmval, (AllocaInst *)left.llvmval);
				result.type = value.type;
				result.llvmval = value.llvmval;
				break;
			}

			// TODO: Both '[]' and '+'(ptr) should use C-style type-based arithmetic
			// rather than byte-based traversal.

			// Handle '[]' for array indexing.
			// TODO: This should happen for both arr & ptr types.
			//		TODO: But what about string array accesses? They're pointers to
			//		arrays rather than pure arrays, which is problematic.
			//		(Solutions: either don't globalise strings [avoid the ptr part],
			//		 or store strings as ptrs rather than ptrs to arrs [avoid arr],
			//		 or strings have operator overloading for '[]' which derefs).
			// TODO: Pointer array indexes are near identical to pointer arith, so the
			// code for both should probably be combined.
			// TODO: Strings should be indexed as UTF-8 some day (instead of per-byte)
			// Since this requires operator overloading anyway, maybe the solution to
			// the ptr to arr problem should be in overloading.
			if (!strcmp(pnode.value, "[]")) {
				PVariable value = generate_lvalue(node);
				if (error)
					break;
				result.type = value.type;
				result.llvmval = builder->CreateLoad(value.llvmval);
				break;
			}


			PValue left = generate_rvalue(pnode.left);
			if (error)
				break;
			PValue right = generate_rvalue(pnode.right);
			if (error)
				break;

			PBaseType *left_base_type = left.type.getBaseType();
			PBaseType *right_base_type = right.type.getBaseType();


			// TODO: Handle pointer (+ other modifiers?) in operations!
			// TODO: Pointer subtraction at some point. Plus, clean up all this type
			// code - particularly with regards to pointers, modifiers, etc.
			if (left.type.is_pointer) {
				if (strcmp(pnode.value, "+")) {
					set_error(node, "pointer types are not yet supported in non-plus "
					          "binary expressions.");
					break;
				}
			} else {
				// TODO: Ensure these binop cast rules are sensible at some point.
				bool cast_left = (!left_base_type->is_signed && right_base_type->is_signed)
				              || (left_base_type->numbits < right_base_type->numbits)
				              || (right_base_type->is_float);
				bool cast_success = false;
				if (cast_left)
					cast_success = implicit_type_convert(&left, right.type);
				else
					cast_success = implicit_type_convert(&right, left.type);

				if (!cast_success) {
					set_error(pnode.right, "type mismatch in binary operation '%s' - expected "
					          "'%s', got '%s'", pnode.value, left.type.to_string(),
					          right.type.to_string());
					break;
				}
			}

			PType type = left.type;
			PBaseType *base_type = type.getBaseType();
			if (!base_type->is_numeric && !type.is_pointer) {
				set_error(pnode.right,
				          "non-numeric type '%s' specified for binary operation",
				          left.type.to_string());
				break;
			}
			
			// TODO: Handle other operators (>=, etc.)
			// NOTE: We use 'ordered' floating point comparisons below. I think this
			// is the correct decision here, but I'm not entirely sure. Check!
			// TODO: Explore if there's a better way to deal with this through LLVM.
			// The repeated float/signed branch code is frustrating.
			result.type = left.type;
			if (!strcmp(pnode.value, "==")) {
				result.type = PType(lookup_base_type("bool"));
				if (base_type->is_float)
					result.llvmval = builder->CreateFCmpOEQ(left.llvmval,
					                                        right.llvmval, "eqtmp");
				else
					result.llvmval = builder->CreateICmpEQ(left.llvmval,
					                                       right.llvmval, "eqtmp");
			} else if (!strcmp(pnode.value, "!=")) {
				result.type = PType(lookup_base_type("bool"));
				if (base_type->is_float)
					result.llvmval = builder->CreateFCmpONE(left.llvmval,
					                                        right.llvmval, "neqtmp");
				else
					result.llvmval = builder->CreateICmpNE(left.llvmval,
					                                       right.llvmval, "neqtmp");
			} else if (!strcmp(pnode.value, "<")) {
				result.type = PType(lookup_base_type("bool"));
				if (base_type->is_float)
					result.llvmval = builder->CreateFCmpOLT(left.llvmval,
					                                        right.llvmval, "lttmp");
				else if (base_type->is_signed)
					result.llvmval = builder->CreateICmpSLT(left.llvmval,
					                                       right.llvmval, "lttmp");
				else
					result.llvmval = builder->CreateICmpULT(left.llvmval,
					                                       right.llvmval, "lttmp");
			} else if (!strcmp(pnode.value, "<=")) {
				result.type = PType(lookup_base_type("bool"));
				if (base_type->is_float)
					result.llvmval = builder->CreateFCmpOLE(left.llvmval,
					                                        right.llvmval, "lttmp");
				else if (base_type->is_signed)
					result.llvmval = builder->CreateICmpSLE(left.llvmval,
					                                       right.llvmval, "lttmp");
				else
					result.llvmval = builder->CreateICmpULE(left.llvmval,
					                                       right.llvmval, "lttmp");
			} else if (!strcmp(pnode.value, ">")) {
				result.type = PType(lookup_base_type("bool"));
				if (base_type->is_float)
					result.llvmval = builder->CreateFCmpOGT(left.llvmval,
					                                        right.llvmval, "gttmp");
				else if (base_type->is_signed)
					result.llvmval = builder->CreateICmpSGT(left.llvmval,
					                                       right.llvmval, "gttmp");
				else
					result.llvmval = builder->CreateICmpUGT(left.llvmval,
					                                       right.llvmval, "gttmp");
			} else if (!strcmp(pnode.value, ">=")) {
				result.type = PType(lookup_base_type("bool"));
				if (base_type->is_float)
					result.llvmval = builder->CreateFCmpOGE(left.llvmval,
					                                        right.llvmval, "gttmp");
				else if (base_type->is_signed)
					result.llvmval = builder->CreateICmpSGE(left.llvmval,
					                                       right.llvmval, "gttmp");
				else
					result.llvmval = builder->CreateICmpUGE(left.llvmval,
					                                       right.llvmval, "gttmp");
			} else if (!strcmp(pnode.value, "+")) {
				if (type.is_pointer)
					result.llvmval = builder->CreateGEP(left.llvmval, right.llvmval);
				else if (base_type->is_float)
					result.llvmval = builder->CreateFAdd(left.llvmval,
					                                     right.llvmval, "addtmp");
				else
					result.llvmval = builder->CreateAdd(left.llvmval,
					                                    right.llvmval, "addtmp");
			} else if (!strcmp(pnode.value, "*")) {
				if (base_type->is_float)
					result.llvmval = builder->CreateFMul(left.llvmval,
					                                     right.llvmval, "multmp");
				else
					result.llvmval = builder->CreateMul(left.llvmval,
					                                    right.llvmval, "multmp");
			} else if (!strcmp(pnode.value, "/")) {
				if (base_type->is_float)
					result.llvmval = builder->CreateFDiv(left.llvmval,
					                                     right.llvmval, "divtmp");
				else if (base_type->is_signed)
					result.llvmval = builder->CreateSDiv(left.llvmval,
					                                     right.llvmval, "divtmp");
				else
					result.llvmval = builder->CreateUDiv(left.llvmval,
					                                     right.llvmval, "divtmp");
			} else if (!strcmp(pnode.value, "-")) {
				if (base_type->is_float)
					result.llvmval = builder->CreateFSub(left.llvmval,
					                                     right.llvmval, "subtmp");
				else
					result.llvmval = builder->CreateSub(left.llvmval,
					                                    right.llvmval, "subtmp");
			} else {
				set_error(node, "unsupported binary operator '%s' for generating rvalue", pnode.value);
			}
			break;
		}
		case NODE_UNARY_OPERATOR:
		{
			auto pnode = *node->toUnaryOperator();

			// TODO: What if we have an unsigned type negated by a unary operator?
			// (Particularly, if we're negating an int literal)

			if (!strcmp(pnode.value, "&")) {
				// TODO: This check is weird. '&(arr[0])', for example, shouldn't fail here.
				if (pnode.operand->type != NODE_IDENTIFIER) {
					set_error(pnode.operand, "invalid operand to '&'");
					break;
				}

				PVariable variable = generate_lvalue(pnode.operand);
				if (error)
					break;

				result.type.indirect_type = (PType *)memory->reserve(sizeof(PType));
				*result.type.indirect_type = variable.type;
				result.type.is_pointer = true;
				result.llvmval = variable.llvmval;
				break;
			} else if (!strcmp(pnode.value, "*")) {
				PVariable value = generate_lvalue(node);
				if (error)
					break;
				result.type = value.type;
				result.llvmval = builder->CreateLoad(value.llvmval);
				break;
			} else if (!strcmp(pnode.value, "+")) {
				PValue value = generate_rvalue(pnode.operand);
				PBaseType *base_type = value.type.getBaseType();
				if (!base_type->is_numeric) {
					set_error(node, "'+' may only be used on numeric types");
					break;
				}

				result = value;
				break;
			}

			set_error(node, "unsupported unary operator '%s' for generating rvalue", pnode.value);
			break;
		}
		case NODE_CAST_OPERATOR:
		{
			auto pnode = *node->toCastOperator();
			PValue value = generate_rvalue(pnode.operand);
			if (error)
				break;
			if (!explicit_type_convert(&value, pnode.type)) {
				set_error(node, "failed to convert type '%s' to '%s'",
				          value.type.to_string(), pnode.type.to_string());
				break;
			}

			result.type = value.type;
			result.llvmval = value.llvmval;
			break;
		}
		case NODE_FUNCTION_CALL:
		{
			auto pnode = *node->toFunctionCall();
			char *function_name = pnode.name->toString()->value;
			Function *function = module->getFunction(function_name);
			PFunction *pfunction = lookup_function(function_name);
			if (!function || !pfunction) {
				set_error(pnode.name, "unknown function referenced '%s'",
				          function_name);
				break;
			}

			size_t args_count = pnode.args.size();
			if (args_count != pfunction->arg_types.size()) {
				set_error(node,
				          "function parameter number mismatch - expected %d, got %d",
				          pfunction->arg_types.size(), args_count);
				break;
			}

			MemoryList<Value *> args(args_count);
			for (size_t i = 0; i < args_count; ++i) {
				PValue arg = generate_rvalue(pnode.args[i]);
				if (error)
					break;
				if (!implicit_type_convert(&arg, pfunction->arg_types[i])) {
					set_error(node, "function parameter mismatch at param %d - "
					          "expected '%s', got '%s'",
					          i + 1, pfunction->arg_types[i].to_string(),
					          arg.type.to_string());
					break;
				}

				assert(arg.type == pfunction->arg_types[i]);
				args.add(arg.llvmval);
			}
			if (error)
				break;

			result.type = pfunction->return_type;
			result.llvmval = builder->CreateCall(function,
			                                     ArrayRef<Value *>(args.getPointer(0),
			                                                       args_count),
			                                     "calltmp");
			break;
		}
		case NODE_CONSTANT_INT:
		{
			// TODO: Need to deal with hex, etc.
			auto pnode = *node->toString();
			char *endptr = pnode.value;
			unsigned long long value = strtoull(pnode.value, &endptr, 10);
			assert(sizeof(value) >= 8);
			if (endptr == pnode.value
			 || (size_t)(endptr - pnode.value) < (strlen(pnode.value))) {
				set_error(node, "failed to parse invalid integer literal");
				break;
			}
			if (errno == ERANGE) {
				set_error(node, "integer literal too large");
				break;
			}

			// Integer literals are unsigned by default, and are put into the smallest
			// type they fit in.
			int numbits = 0;
			if (value <= 0xff) {
				result.type = PType(lookup_base_type("uint8"));
			} else if (value <= 0xffff) {
				result.type = PType(lookup_base_type("uint16"));
			} else if (value <= 0xffffffff) {
				result.type = PType(lookup_base_type("uint32"));
			} else if (value <= 0xffffffffffffffff) {
				result.type = PType(lookup_base_type("uint64"));
			} else {
				set_error(node, "integer literal too large");
				break;
			}
			numbits = result.type.getBaseType()->numbits;

			result.llvmval = ConstantInt::get(getGlobalContext(),
			                                  APInt(numbits, value, false));
			assert(result.llvmval->getType() == result.type.getLLVMType());
			break;
		}
		case NODE_CONSTANT_BOOL:
		{
			auto pnode = *node->toInteger();
			result = get_boolean_value(pnode.value);
			break;
		}
		case NODE_CONSTANT_FLOAT:
		{
			// TODO: This seems like a pretty terrible way to initialize an APFloat.
			// TODO: Also, we probably want to deal with oversized floats or whatever
			// here.
			auto pnode = *node->toString();
			result.type = PType(lookup_base_type("float64"));
			APFloat number(0.0);
			number.convertFromString(pnode.value, APFloat::rmNearestTiesToEven);
			result.llvmval = ConstantFP::get(getGlobalContext(), number);
			break;
		}
		case NODE_CONSTANT_STRING:
		{
			auto pnode = *node->toString();
			char *str = pnode.value;
			size_t string_length = strlen(str);

			PType *byte_type = (PType *)memory->reserve(sizeof(PType));
			*byte_type = PType(lookup_base_type("uint8"));
			PType *array_type = (PType *)memory->reserve(sizeof(PType));
			*array_type = PType(NULL, false, string_length + 1, byte_type);
			result.type = PType(NULL, true, 0, array_type);
			result.llvmval = builder->CreateGlobalString(StringRef(str,
			                                                       string_length));
			break;
		}
		case NODE_IDENTIFIER:
		{
			auto pnode = *node->toString();
			PVariable value = generate_lvalue(node);
			if (error)
				break;
			result.type = value.type;
			result.llvmval = builder->CreateLoad(value.llvmval, pnode.value);
			break;
		}
		default:
			set_error(node, "failed to generate expression for ASTNode");
			break;
	}

	return result;
}

PFunction CodeGenerator::generate_function(ASTNode *node) {
	PFunction result;

	switch (node->type) {
		case NODE_FUNCTION_SIGNATURE:
		{
			auto pnode = *node->toFunctionSignature();
			Environment *previous_env = env;
			env = pnode.env;

			size_t args_count = pnode.args.size();
			MemoryList<Type *> args(args_count);
			for (size_t i = 0; i < args_count; ++i) {
				auto arg = *pnode.args[i]->toVariableDeclaration();

				PType type = arg.type;
				result.arg_types.add(type);
				args.add(type.getLLVMType());
			}
			if (error)
				break;

			char *function_name = pnode.name->toString()->value;
			PType type = pnode.type;
			FunctionType *function_type;
			Function *function;
			function_type = FunctionType::get(type.getLLVMType(),
			                                  ArrayRef<Type *>(args.getPointer(0),
			                                                   args_count),
			                                  false);
			function = Function::Create(function_type, Function::ExternalLinkage,
			                            function_name, module);
			function->addFnAttr(Attribute::NoUnwind);

			// If the name we got back isn't the one we assigned, there was a conflict
			if (function->getName() != function_name) {
				// Erase the just-created signature, and get the previous one.
				function->eraseFromParent();
				function = module->getFunction(function_name);

				if (!function->empty()) {
					set_error(node, "redefinition of function is not allowed");
					break;
				}

				PFunction *pfunction = lookup_function(function_name);
				if (!pfunction) {
					set_error(pnode.name,
					          "conflict between LLVM and function table state");
					break;
				}

				if (pfunction->return_type != type) {
					set_error(node,
					          "redefinition of function with differing return type");
					break;
				}

				if (pfunction->arg_types.size() != result.arg_types.size()) {
					set_error(node, "redefinition of function with parameter number "
					          "mismatch - expected %d, got %d",
					          pfunction->arg_types.size(), args_count);
					break;
				}
				for (size_t i = 0; i < pfunction->arg_types.size(); ++i) {
					PType expected_type = pfunction->arg_types[i];
					PType actual_type = result.arg_types[i];
					if (actual_type != expected_type) {
						set_error(node, "redefinition of function with parameter mis-match "
						          "at param %d", i);
						break;
					}
				}
				if (error)
					break;

				result = *pfunction;
			} else {
				result.llvmval = function;
				result.return_type = pnode.type;
				previous_env->function_table.set(function_name, result);
			}

			env = previous_env;
			break;
		}
		case NODE_FUNCTION:
		{
			auto pnode = *node->toFunction();
			PFunction function = generate_function(pnode.signature);
			if (error)
				break;

			BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry",
			                                    function.llvmval);
			builder->SetInsertPoint(BB);

			Environment *prev_env = env;
			env = pnode.signature->toFunctionSignature()->env;

			{
				auto psignature = *pnode.signature->toFunctionSignature();
				MemoryList<ASTNode *> args = psignature.args;
				Function *funcval = function.llvmval;
				Function::arg_iterator it;
				size_t i;
				for (it = funcval->arg_begin(), i = 0; i < args.size(); ++i, ++it) {
					generate_lvalue(args[i]);
					if (error)
						break;

					auto arg = *args[i]->toVariableDeclaration();
					char *param_name = arg.name->toString()->value;
					it->setName(param_name);
					PVariable *param = lookup_symbol(param_name);
					builder->CreateStore(it, param->llvmval);
				}
			}

			generate_statement(pnode.body);
			if (error)
				break;

			env = prev_env;

			result = function;
			verifyFunction(*function.llvmval);
			fpm->run(*function.llvmval);
			break;
		}
		default:
			set_error(node, "unexpected token for function code generation");
			break;
	}

	return result;
}

void CodeGenerator::generate_statement(ASTNode *node) {
	switch (node->type) {
		case NODE_STATEMENTS:
		{
			auto pnode = *node->toStatements();
			for (size_t i = 0; i < pnode.children.size(); ++i) {
				generate_statement(pnode.children[i]);
				if (error)
					break;
			}
			break;
		}
		case NODE_BLOCK:
		{
			auto pnode = *node->toBlock();
			Environment *prev_env = env;
			env = pnode.env;
			generate_statement(pnode.statements);
			env = prev_env;
			break;
		}
		case NODE_UNARY_OPERATOR:
		case NODE_BINARY_OPERATOR:
			generate_rvalue(node);
			break;
		case NODE_FUNCTION_SIGNATURE:
		case NODE_FUNCTION:
			generate_function(node);
			break;
		case NODE_FUNCTION_CALL:
			generate_rvalue(node);
			break;
		case NODE_VARIABLE_DECLARATION:
			generate_lvalue(node);
			break;
		case NODE_IF:
		{
			auto pnode = *node->toConditional();
			PValue true_val = get_boolean_value(true);
			PValue cond = generate_rvalue(pnode.condition);
			if (error)
				break;
			if (cond.type != true_val.type) {
				set_error(pnode.condition,
				          "unsupported type for if statement condition: '%s'",
				          cond.type.to_string());
				break;
			}

			cond.llvmval = builder->CreateICmpEQ(cond.llvmval,
			                                     true_val.llvmval, "ifcond");

			Function *function;
			BasicBlock *then, *otherwise, *merge;

			function = builder->GetInsertBlock()->getParent();
			then = BasicBlock::Create(getGlobalContext(), "then", function);
			otherwise = BasicBlock::Create(getGlobalContext(), "else", function);
			merge = BasicBlock::Create(getGlobalContext(), "ifcont", function);
			builder->CreateCondBr(cond.llvmval, then, otherwise);

			builder->SetInsertPoint(then);
			generate_statement(pnode.then);
			if (error)
				break;
			builder->CreateBr(merge);

			builder->SetInsertPoint(otherwise);
			if (pnode.otherwise) {
				generate_statement(pnode.otherwise);
				if (error)
					break;
			}
			builder->CreateBr(merge);

			builder->SetInsertPoint(merge);
			break;
		}
		case NODE_WHILE_LOOP:
		{
			auto pnode = *node->toConditional();
			Function *function;
			BasicBlock *preloop, *loop, *after;

			function = builder->GetInsertBlock()->getParent();
			preloop = BasicBlock::Create(getGlobalContext(), "prewhile", function);
			loop = BasicBlock::Create(getGlobalContext(), "while", function);
			after = BasicBlock::Create(getGlobalContext(), "afterwhile", function);
			builder->CreateBr(preloop);

			builder->SetInsertPoint(preloop);
			PValue true_val = get_boolean_value(true);
			PValue cond = generate_rvalue(pnode.condition);
			if (error)
				break;

			if (cond.type != true_val.type) {
				set_error(pnode.condition,
				          "unsupported type for while statement condition: %s",
				          cond.type.to_string());
				break;
			}

			cond.llvmval = builder->CreateICmpEQ(cond.llvmval,
			                                     true_val.llvmval,
			                                     "whilecond");
			builder->CreateCondBr(cond.llvmval, loop, after);

			// TODO: Handle 'otherwise' branch (while..else)

			builder->SetInsertPoint(loop);
			generate_statement(pnode.then);
			if (error)
				break;
			builder->CreateBr(preloop);
			builder->SetInsertPoint(after);
			break;
		}
		case NODE_RETURN:
		{
			auto pnode = *node->toUnaryOperator();
			PValue val;
			if (pnode.operand) {
				val = generate_rvalue(pnode.operand);
				if (error)
					break;
			} else {
				val.llvmval = NULL;
			}

			PFunction *function = lookup_function(env->current_function);
			if (!implicit_type_convert(&val, function->return_type)) {
				set_error(pnode.operand,
				          "type mismatch in return statement - expected '%s', got '%s'",
				          function->return_type.to_string(), val.type.to_string());
				break;
			}

			builder->CreateRet(val.llvmval);
			break;
		}
		case NODE_DO_LOOP:
		case NODE_UNTIL_LOOP:
		case NODE_FOR_LOOP:
		case NODE_BREAK:
		case NODE_CONTINUE:
			set_error(node, "feature not yet implemented!");
			break;
		case NODE_CONSTANT_INT:
		case NODE_CONSTANT_FLOAT:
		case NODE_CONSTANT_STRING:
			break;
		default:
			set_error(node, "failed to generate statement for ASTNode");
			break;
	}
}

Module *CodeGenerator::generate() {
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
	IRBuilder<> builder = IRBuilder<>(getGlobalContext());
	this->builder = &builder;
	module = new Module("program", getGlobalContext());

	error = NULL;
	errnode = NULL;

	// TODO: Add module metadata here (inc. DataLayout)

	// TODO: Think about alignment at some point. It seems like LLVM gives a lot
	// of support for data alignment, but we're not currently using it.

	// TODO: Think about (and extend) code passes (optimisation, etc.)
	// Additionally, the order of these needs to be properly thought about!
	// TODO: Perform optimisations based on passed optimisation flags.
	FunctionPassManager fpm(module);
	fpm.add(createBasicAliasAnalysisPass());
	fpm.add(createPromoteMemoryToRegisterPass());
	fpm.add(createReassociatePass());
	fpm.add(createConstantPropagationPass());
	fpm.add(createDeadCodeEliminationPass());
	fpm.add(createGVNPass());
	fpm.add(createCFGSimplificationPass());
	fpm.add(createInstructionCombiningPass());
	fpm.doInitialization();
	this->fpm = &fpm;

	generate_statement(root);
	if (error)
		return NULL;
	verifyModule(*module);

	return module;
}
