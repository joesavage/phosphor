#ifndef HASH_MAP_H
#define HASH_MAP_H

#include <stdlib.h>
#include <string.h>

#include "helpers.h"

template <typename T>
struct HashNode {
	char key[128];
	T value;
};

// Optimised for insertions and lookups, not for deletions.
template <typename T>
struct HashMap {
	size_t size;
	size_t count;
	HashNode<T> *buckets;

	HashMap(size_t initial_size = 128);
	void destroy();
	bool resize(size_t new_size);
	bool set(const char *key, T value);
	bool exists(const char *key);
	HashNode<T> *operator[](const char *key);
};
 
// Implementation
static unsigned int hash(const char *string) {
	if (!string)
		return 0;
	unsigned int hash = 0;
	// TODO: Improve hashing algorithm?
	while (*string)
		hash = (hash * 101) + *string++;
	return hash;
}

template <typename T>
HashMap<T>::HashMap(size_t initial_size) {
	size = initial_size;
	count = 0;
	buckets = NULL;
}

template <typename T>
void HashMap<T>::destroy() {
	free(buckets);
}

template <typename T>
bool HashMap<T>::resize(size_t new_size) {
	if (new_size == size)
		return true;
	else if (new_size < size)
		return false;
	
	HashNode<T> *old_buckets = buckets;
	size_t old_size = size;
	size = new_size;
	buckets = (HashNode<T> *)heap_alloc(sizeof(buckets[0]) * new_size);
	// NOTE: Wrapping around 'size' means we have to re-add all the hash nodes
	// under the new hash table size - O(n).
	for (size_t i = 0; i < old_size; ++i) {
		if (old_buckets[i].key[0] != '\0')
			buckets[hash(old_buckets[i].key) % size] = old_buckets[i];
	}
	free(old_buckets);

	return true;
}

// TODO: Factor out the searching code. Too much code repetition for my liking.

template <typename T>
bool HashMap<T>::set(const char *key, T value) {
	if (!key || !size)
		return false;

	if (!buckets) {
		buckets = (HashNode<T> *)heap_alloc(sizeof(HashNode<T>) * size);
		memset(buckets, 0, sizeof(HashNode<T>) * size);
	}

	HashNode<T> node;
	if (strlen(key) >= sizeof(node.key)) {
		fatal_error("HashMap key length greater/eq than %lu!\n", sizeof(node.key));
		return false;
	}

	// If the map is over half full, bump up the size.
	if (count * 1.25f > size)
		resize(size * 2);

	memcpy(node.key, key, strlen(key) + 1);
	node.value = value;
	size_t index = hash(key) % size;
	while (buckets[index].key[0] != '\0') {
		if (!strcmp(buckets[index].key, key))
			break;
		index = (index + 1) % size;
	}
	buckets[index] = node;
	count++;

	return true;
}

template <typename T>
HashNode<T> *HashMap<T>::operator[](const char *key) {
	if (!buckets || !key || !size)
		return NULL;

	size_t index = hash(key) % size;
	while (strcmp(buckets[index].key, key)) {
		if (buckets[index].key[0] == '\0')
			return NULL;
		index = (index + 1) % size;
	}
	return &buckets[index];
}

template <typename T>
bool HashMap<T>::exists(const char *key) {
	if (!buckets || !key || !size)
		return false;

	size_t index = hash(key) % size;
	while (strcmp(buckets[index].key, key)) {
		if (buckets[index].key[0] == '\0')
			return false;
		index = (index + 1) % size;
	}

	return true;
}

#endif
