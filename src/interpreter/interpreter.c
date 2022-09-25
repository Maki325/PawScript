#include "interpreter.h"

bool interpretToken(Program *program, void **eax, size_t i, size_t *iPtr, HashTable *table, const char *name, const char **namePtr) {
  (void) eax;
  (void) iPtr;
  (void) table;
  (void) name;
  (void) namePtr;

  Token *token = program->instructions[i];
  switch(token->type) {
    default: {
      printf("Token: (%s, %d)\n", getTokenTypeName(token->type), token->type);
      ASSERT(false, "Not all operations are implemented in interpret!");
    }
  }
  return true;
}

void interpretScope(Program *program, void** eax, HashTable *table) {
  ASSERT(TOKEN_COUNT == 20, "Not all operations are implemented in interpret!");
  const char *name = NULL;

  (void) name;

  for(size_t i = 0;i < program->count;i++) {
    interpretToken(program, eax, i, &i, table, name, &name);
  }
}

void interpret(Program *program) {
  HashTable *table = createHashTable(256);
  void *eax = NULL;
  interpretScope(program, &eax, table);
  deleteHashTable(table);
}
