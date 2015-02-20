#ifndef HELPERS_H
#define HELPERS_H

void *fatal_error(const char *format, ...);
void unreachable_code_path();
void *heap_alloc(size_t size);
void *heap_realloc(void *oldMemory, size_t newSize);

#endif
