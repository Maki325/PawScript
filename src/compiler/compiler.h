#ifndef PS_COMPILER_H
#define PS_COMPILER_H
#include "../includes.h"
#include "../tokenizer.h"

typedef struct CompileVariable
{
  Type type;
  void *initialValue;
  size_t usageCount;
} CompileVariable;

char *getInitializedType(Type type);
char *getUninitializedType(Type type);
CompileVariable *createVariable(Type type, void *initialValue);

void addPrintFunction(FILE *out);
void prepareFileForCompile(FILE *out);
void postCompile(FILE *out);
bool generateBinaryOperationAsm(Token *token, HashTable *table, FILE *out, char *error);
void generateProgramAsm(Program *program, HashTable *table, FILE *out, char *error);
void generateAsm(Program *program, const char *basename, bool silent, char *error);
void compile(const char *basename, bool silent);
void runProgram(const char *basename, bool silent);

#endif
