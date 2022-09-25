#ifndef OPTIMIZER_H_
#define OPTIMIZER_H_
#include "../tokenizer.h"

void optimizeConstVariables(Program *program, HashTable *constValues);

void optimizeProgram(Program *program);

#endif // OPTIMIZER_H_