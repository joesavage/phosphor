#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "memoryarena.h"
#include "helpers.h"

MemoryArenaChunk::MemoryArenaChunk(size_t initial_size) {
	data = NULL;
	size = initial_size;
	cursor = 0;
}

char *MemoryArenaChunk::reserve(size_t length) {
	if (size == 0) size = MEMORY_ARENA_CHUNK_DEFAULT_SIZE;
	if ((cursor + length) > size || cursor > size)
		return NULL;
	if (data == NULL)
		data = (char *)heap_alloc(size);

	char *destination = data + cursor;
	cursor += length + 1;

	return destination;
}

void MemoryArenaChunk::free() {
	if (data) {
		::free((void *)data);
		data = NULL;
	}
	cursor = 0;
	size = 0;
}


MemoryArena::MemoryArena(size_t initial_init_size) {
	chunk_init_size = initial_init_size;
	chunk_cursor = 0;
	chunk_cursor = 0;
}

char *MemoryArena::reserve(size_t length) {
	if (!chunks.getPointer(chunk_cursor))
		chunks.add(MemoryArenaChunk());

	// TODO:/NOTE: Right now, 'wasted' space near the end of chunks isn't used.

	char *destination;
	if (!(destination = chunks.getPointer(chunk_cursor)->reserve(length))) {
		size_t new_chunk_size = length > chunk_init_size ? length : chunk_init_size;
		chunk_cursor = chunks.add(MemoryArenaChunk(new_chunk_size));
		destination = chunks.getPointer(chunk_cursor)->reserve(length);
	}

	return destination;
}

char *MemoryArena::add(void *data, size_t length) {
	char *destination = reserve(length);
	memcpy((void *)destination, data, length);
	return destination;
}

char *MemoryArena::strndup(const char *data, size_t max_length) {
	size_t length = strlen(data);
	max_length = max_length < length ? max_length : length;
	char *destination = reserve(max_length + 1);
	memcpy((void *)destination, data, max_length);
	destination[max_length] = '\0';
	return destination;
}

char *MemoryArena::strdup(const char *data) {
	return strndup(data, strlen(data));
}

void MemoryArena::free() {
	for (size_t i = 0; i < chunk_cursor; ++i)
		chunks.getPointer(chunk_cursor)->free();
	chunks.free();
}
