#ifndef MEMORY_LIST_H
#define MEMORY_LIST_H

#include <string.h>
#include <stdlib.h>

#include "helpers.h"

#define MEMORY_LIST_DEFAULT_SIZE 16

template <typename T>
struct MemoryList {
	MemoryList(size_t initial_size = MEMORY_LIST_DEFAULT_SIZE);
	size_t reserve(size_t length);
	size_t add(T source);
	inline size_t size();
	T *getPointer(size_t index);
	T operator[](size_t index);
	void free();

private:
	T *_data;
	size_t _size;
	size_t _cursor;
};

template <typename T>
MemoryList<T>::MemoryList(size_t initial_size) {
	_data = NULL;
	_size = initial_size;
	_cursor = 0;
}

template <typename T>
void MemoryList<T>::free() {
	if (_data)
		::free((void *)_data);
	_data = NULL;
	_size = 0;
	_cursor = 0;
}

#include <stdio.h>

template <typename T>
size_t MemoryList<T>::reserve(size_t length) {
	if (_size == 0)
		_size = MEMORY_LIST_DEFAULT_SIZE;
	if ((_cursor + length) > _size || _cursor > _size) {
		size_t old_size = _size;
		while ((_cursor + length) > _size || _cursor > _size)
			_size *= 2;

		T *old__data = _data;
		_data = (T *)heap_alloc(_size);
		if (old__data) {
			memcpy((void *)_data, old__data, old_size);
			::free(old__data);
		}
	}

	if (!_data)
		_data = (T *)heap_alloc(_size);

	size_t destination = _cursor;
	_cursor += length + 1;

	return destination / sizeof(T); // TODO: This mixing of T-rel and byte-rel is dumb.
}

template <typename T>
size_t MemoryList<T>::add(T source) {
	size_t destination = reserve(sizeof(T));
	*(_data + destination) = source;
	return destination;
}

template <typename T>
size_t MemoryList<T>::size() {
	return _cursor / sizeof(T);
}

template <typename T>
T *MemoryList<T>::getPointer(size_t index) {
	if (!_data)
		return NULL;
	return (_data + index);
}

template <typename T>
T MemoryList<T>::operator[](size_t index) {
	return *(_data + index);
}


#endif
