#ifndef LIST_H_
#define LIST_H_
#include "../includes.h"

#define DEFAULT_LIST_CAPACITY 256

typedef struct List {
  void **elements;
  size_t size;
  size_t capacity;
} List;

List *createList(size_t capacity);
void deleteList(List *list);

void expandList(List *list);
void addElementToList(List *list, void *element);


#endif // LIST_H_