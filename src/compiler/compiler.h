#ifndef PS_COMPILER_H
#define PS_COMPILER_H
#include "../includes.h"
#include "../tokenizer.h"

typedef struct CompileVariable
{
  Type type;
  void *initialValue;
} CompileVariable;

char *getInitializedType(Type type);
char *getUninitializedType(Type type);
CompileVariable *createVariable(Type type, void *initialValue);

void addPrintFunction(FILE *out);
void prepareFileForCompile(FILE *out);
void postCompile(FILE *out);
bool generateBinaryOperationAsm(Token *token, FILE *out, const char *error);
void generateAsm(Program *program, FILE *out, const char *error);
void compile(const char *basename);
void runProgram(const char *basename);

#endif
