#include "list.h"

List *createList(size_t capacity) {
  if(capacity == 0) capacity = DEFAULT_LIST_CAPACITY;

  List *list = malloc(sizeof(List));
  list->capacity = capacity;
  list->size = 0;
  list->elements = calloc(list->capacity, sizeof(void*));

  return list;
}

void deleteList(List *list) {
  for(size_t i = 0;i < list->size;i++) {
    free(list->elements[i]);
  }
  free(list->elements);
  free(list);
}

void expandList(List *list) {
  if(list->size <= list->capacity / 2) return;
  
  list->capacity = list->capacity + min(1000, list->capacity);
  void **data = calloc(list->capacity, sizeof(void*));

  for(size_t i = 0;i < list->size;i++) {
    data[i] = list->elements[i];
  }
  free(list->elements);
  list->elements = data;
}

void addElementToList(List *list, void *element) {
  expandList(list);

  list->elements[list->size++] = element;
}
