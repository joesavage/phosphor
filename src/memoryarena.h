#ifndef MEMORY_ARENA_H
#define MEMORY_ARENA_H

#include "memorylist.hpp"

#define MEMORY_ARENA_CHUNK_DEFAULT_SIZE 256

struct MemoryArenaChunk {
	char *data;
	size_t size;
	size_t cursor;
	MemoryArenaChunk(size_t initial_size = MEMORY_ARENA_CHUNK_DEFAULT_SIZE);
	char *reserve(size_t length);
	void free();
};

// Designed for chunked memory allocations with zero deletions, without pointer
// invalidation issues.
struct MemoryArena {
	size_t chunk_cursor;
	MemoryList<MemoryArenaChunk> chunks;

	size_t chunk_init_size;

	MemoryArena(size_t initial_init_size = MEMORY_ARENA_CHUNK_DEFAULT_SIZE);
	char *reserve(size_t length);
	char *add(void *data, size_t length);
	char *strndup(const char *data, size_t max_length);
	char *strdup(const char *data);
	void free();
};

#endif
