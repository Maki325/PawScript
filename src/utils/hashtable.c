#include "hashtable.h"

HashTable *createHashTable(size_t capacity) {
  if(capacity == 0) capacity = DEFAULT_HASHTABLE_CAPACITY;
  HashTable *hashTable = malloc(sizeof(HashTable));
  hashTable->capacity = capacity;
  hashTable->size = 0;
  hashTable->elements = calloc(hashTable->capacity, sizeof(HashTableElement));

  return hashTable;
}

HashTable *createHashTableFrom(HashTable *origin) {
  if(!origin) return createHashTable(DEFAULT_HASHTABLE_CAPACITY);
  HashTable *hashTable = malloc(sizeof(HashTable));
  hashTable->capacity = origin->capacity;
  hashTable->size = origin->size;
  hashTable->elements = calloc(hashTable->capacity, sizeof(HashTableElement));
  for(size_t i = 0; i < hashTable->capacity;i++) {
    if(!origin->elements[i].key) continue;
    hashTable->elements[i].key   = strdup(origin->elements[i].key);
    hashTable->elements[i].value = origin->elements[i].value;
  }

  return hashTable;
}

void deleteHashTable(HashTable* hashTable) {
  for(size_t i = 0; i < hashTable->size;i++) {
    free(hashTable->elements[i].value);
  }
  free(hashTable->elements);
  free(hashTable);
}

uint64_t hashKeyLength(const char* key, size_t length) {
  uint64_t hash = FNV_OFFSET;
  for (size_t i = 0;i < length;i++) {
    hash ^= (uint64_t) (unsigned char) key[i];
    hash *= FNV_PRIME;
  }
  return hash;
}
uint64_t hashKey(const char* key) {
  return hashKeyLength(key, strlen(key));
}

void *getElementFromHashTableLength(HashTable *hashTable, const char *key, size_t length) {
  uint64_t hash = hashKeyLength(key, length);
  size_t index = hash & (hashTable->capacity - 1);
  
  while(hashTable->elements[index].key != NULL) {
    if(strcmp(hashTable->elements[index].key, key) == 0) {
      return hashTable->elements[index].value;
    }
    index++;
    if(index >= hashTable->capacity) index = 0;
  }

  return NULL;
}
void *getElementFromHashTable(HashTable *hashTable, const char *key) {
  return getElementFromHashTableLength(hashTable, key, strlen(key));
}

bool existsElementInHashTable(HashTable *hashTable, const char *key) {
  uint64_t hash = hashKey(key);
  size_t index = hash & (hashTable->capacity - 1);
  
  while(hashTable->elements[index].key != NULL) {
    if(strcmp(hashTable->elements[index].key, key) == 0) {
      return true;
    }
    index++;
    if(index >= hashTable->capacity) index = 0;
  }

  return false;
}

void expandHashTable(HashTable *hashTable) {
  if(hashTable->size <= hashTable->capacity / 2) return;

  size_t oldCapacity = hashTable->capacity;
  hashTable->capacity = hashTable->capacity + min(1000, hashTable->capacity);
  HashTableElement *elements = hashTable->elements;
  hashTable->elements = calloc(hashTable->capacity, sizeof(HashTableElement));

  hashTable->size = 0;
  for(size_t i = 0; i < oldCapacity;i++) {
    if(elements[i].key == NULL) continue;
    setElementInHashTable(hashTable, elements[i].key, elements[i].value);
  }
  free(elements);
}

void setElementInHashTable(HashTable *hashTable, const char *key, void *value) {
  if(hashTable->size > hashTable->capacity / 2) {
    expandHashTable(hashTable);
  }

  uint64_t hash = hashKey(key);
  size_t index = hash & (hashTable->capacity - 1);

  while(hashTable->elements[index].key != NULL) {
    if(strcmp(hashTable->elements[index].key, key) == 0) {
      hashTable->elements[index].value = value;
      return;
    }
    index++;
    if(index >= hashTable->capacity) index = 0;
  }

  hashTable->size++;
  hashTable->elements[index].key = key;
  hashTable->elements[index].value = value;
}

void *removeElementFromHashTable(HashTable *hashTable, const char *key) {
  uint64_t hash = hashKey(key);
  size_t index = hash & (hashTable->capacity - 1);
  
  while(hashTable->elements[index].key != NULL) {
    if(strcmp(hashTable->elements[index].key, key) == 0) {
      void *value = hashTable->elements[index].value;

      hashTable->elements[index].key = NULL;
      hashTable->elements[index].value = NULL;
      hashTable->size--;

      return value;
    }
    index++;
    if(index >= hashTable->capacity) index = 0;
  }

  return NULL;
}
