#include "interpreter.h"

bool interpretToken(Program *program, void **eax, size_t i, size_t *iPtr, HashTable *table, const char *name, const char **namePtr, char *error) {
  Token *token = program->instructions[i];
  switch(token->type) {
    default: {
      printf("Token: (%s, %d)\n", getTokenTypeName(token->type), token->type);
      ASSERT(false, "Not all operations are implemented in interpret!");
    }
  }
  return true;
}

void interpretScope(Program *program, void** eax, char *error, HashTable *table) {
  ASSERT(TOKEN_COUNT == 20, "Not all operations are implemented in interpret!");
  const char *name = NULL;
  for(size_t i = 0;i < program->count;i++) {
    interpretToken(program, eax, i, &i, table, name, &name, error);
    if(error[0] != '\0') {
      return;
    }
  }
}

void interpret(Program *program, char *error) {
  HashTable *table = createHashTable(255);
  void *eax = NULL;
  interpretScope(program, &eax, error, table);
  deleteHashTable(table);
}
