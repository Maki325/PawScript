#ifndef HASHTABLE_H
#define HASHTABLE_H
#include "../includes.h"

#define DEFAULT_HASHTABLE_CAPACITY 256
#define FNV_OFFSET 14695981039346656037UL
#define FNV_PRIME 1099511628211UL

typedef struct HashTableElement {
  const char *key;
  void *value;
} HashTableElement;

typedef struct HashTable {
  HashTableElement *elements;
  size_t capacity;
  size_t size;
} HashTable;

HashTable *createHashTable(size_t capacity);
HashTable *createHashTableFrom(HashTable *origin);
void deleteHashTable(HashTable* hashTable);

uint64_t hashKey(const char* key);
uint64_t hashKeyLength(const char* key, size_t length);
void *getElementFromHashTable(HashTable *hashTable, const char *key);
void *getElementFromHashTableLength(HashTable *hashTable, const char *key, size_t length);

void expandHashTable(HashTable *hashTable);
void setElementInHashTable(HashTable *hashTable, const char *key, void *value);
bool existsElementInHashTable(HashTable *hashTable, const char *key);
void *removeElementFromHashTable(HashTable *hashTable, const char *key);


#endif