#ifndef PS_INTERPRETER_H
#define PS_INTERPRETER_H
#include "../tokenizer/tokenizer.h"
#include "../utils/utils.h"

bool interpretToken(Program *program, void **eax, size_t i, size_t *iPtr, HashTable *table, const char *name, const char **namePtr);
void interpretScope(Program *program, void **eax, HashTable *parent);
void interpret(Program *program);

#endif