#ifndef PS_INTERPRETER_H
#define PS_INTERPRETER_H
#include "../tokenizer.h"
#include "../utils/utils.h"

void *interpretBinaryOperation(Token *token, HashTable *table, const char *name, const char **namePtr, char *error);
bool interpretToken(Program *program, size_t i, HashTable *table, const char *name, const char **namePtr, char *error);
void interpret(Program *program, char *error);

#endif