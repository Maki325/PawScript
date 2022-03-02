#ifndef PS_COMPILER_H
#define PS_COMPILER_H
#include "../includes.h"
#include "../tokenizer.h"

typedef struct CompileVariable
{
  Type type;
  void *initialValue;
  bool assignType;
} CompileVariable;

char *getInitializedType(Type type);
char *getUninitializedType(Type type);
CompileVariable *createVariable(Type type, bool assignType, void *initialValue);

void addPrintFunction(FILE *out);
void prepareFileForCompile(FILE *out);
void postCompile(FILE *out);
bool generateBinaryOperationAsm(Token *token, FILE *out, char *error);
void generateProgramAsm(Program *program, HashTable *table, FILE *out, char *error);
void generateAsm(Program *program, FILE *out, char *error);
void compile(const char *basename);
void runProgram(const char *basename);

#endif
