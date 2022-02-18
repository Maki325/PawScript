#ifndef PS_INTERPRETER_H
#define PS_INTERPRETER_H
#include "../tokenizer.h"

bool interpretToken(Program *program, size_t i, HashTable *table, char *name, char **namePtr, char *error);
void interpret(Program *program, char *error);

#endif