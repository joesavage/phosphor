#ifndef MEMORY_LIST_H
#define MEMORY_LIST_H

#include <string.h>
#include <stdlib.h>

#include "helpers.h"

#define MEMORY_LIST_DEFAULT_SIZE 32

template <typename T>
struct MemoryList {
	T *data;
	size_t size;
	size_t cursor;

	MemoryList(size_t initial_size = MEMORY_LIST_DEFAULT_SIZE);
	size_t reserve(size_t length);
	size_t add(T source);
	T *getPointer(size_t index);
	T *operator[](size_t index);
	void free();
};

template <typename T>
MemoryList<T>::MemoryList(size_t initial_size) {
	data = NULL;
	size = initial_size;
	cursor = 0;
}

template <typename T>
void MemoryList<T>::free() {
	if (data)
		::free((void *)data);
	data = NULL;
	size = 0;
	cursor = 0;
}

#include <stdio.h>

template <typename T>
size_t MemoryList<T>::reserve(size_t length) {
	if (size == 0)
		size = MEMORY_LIST_DEFAULT_SIZE;
	if ((cursor + length) > size || cursor > size) {
		size_t old_size = size;
		while ((cursor + length) > size || cursor > size)
			size *= 2;

		T *old_data = data;
		data = (T *)heap_alloc(size);
		if (old_data) {
			memcpy((void *)data, old_data, old_size);
			::free(old_data);
		}
	}

	if (!data)
		data = (T *)heap_alloc(size);

	size_t destination = cursor;
	cursor += length + 1;

	return destination / sizeof(T); // TODO: This mixing of T-rel and byte-rel is dumb.
}

template <typename T>
size_t MemoryList<T>::add(T source) {
	size_t destination = reserve(sizeof(T));
	*(data + destination) = source;
	return destination;
}

template <typename T>
T *MemoryList<T>::getPointer(size_t index) {
	if (!data)
		return NULL;
	return (data + index);
}

template <typename T>
T *MemoryList<T>::operator[](size_t index) {
	return *(data + index);
}


#endif
