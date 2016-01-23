#include "helpers.h"

#include <stdarg.h>
#include <stdio.h>

void *fatal_error(const char *format, ...) {
	va_list arglist;
	va_start(arglist, format);
	vfprintf(stderr, format, arglist);
	va_end(arglist);
	exit(1);
}

void unreachable_code_path() {
	fatal_error("fatal error: code path should not be executed.\n");
}

void *heap_alloc(size_t size) {
	void *memory = malloc(size);
	if (memory == NULL)
		fatal_error("fatal error: out of memory.\n");

	return memory;
}

void *heap_realloc(void *oldMemory, size_t newSize) {
	void *memory = realloc(oldMemory, newSize);
	if (memory == NULL)
		fatal_error("fatal error: realloc failed.\n");

	return memory;
}
