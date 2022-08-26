#ifndef PS_COMPILER_H
#define PS_COMPILER_H
#include "../includes.h"
#include "../tokenizer.h"

typedef struct CompilerOptions {
  Program *program;
  FILE *output;
  bool silent;
  const char *basename;
} CompilerOptions;

/**
 * 
 * @param type The type of the variable
 * @param offset The offset of the variable in bytes
 */
typedef struct CompileVariable {
  NameData *nameData;
  Type type;
  int offset;
} CompileVariable;

char *getInitializedType(Type type);
char *getUninitializedType(Type type);
CompileVariable *createVariable(NameData *nameData, int offset);

void addPrintFunction(FILE *out);
void prepareFileForCompile(FILE *out);
void postCompile(FILE *out);
bool generateBinaryOperationAsm(Token *token, FILE *out, char *error);

/**
 * @brief Generates the asm code of the function
 * 
 * @param compilerOptions The pointer to CompilerOptions struct
 * @param functionData The pointer to FunctionDefinition of which we want to generate the ASM code
 * @param offset The current offset of the stack
 * @param parentVariables HashTable of all the variables currently available in the scope
 * 
 * @throw Exits the program if there's an error
 */
void generateFunctionAsm(CompilerOptions *compilerOptions, FunctionDefinition *functionData, int offset, HashTable *parentVariables);

/**
 * Generates the asm code of the program
 * 
 * @param compilerOptions The pointer to CompilerOptions
 * @param program The pointer to Program of which we want to generate the ASM code
 * @param offset The current offset of the stack
 * @param parentVariables HashTable of all the variables currently available in the scope
 * @param globalVariables Nullable HashTable of all the global variables
 * @param out Pointer of the stream to which we want to output the ASM to
 * 
 * @throw Exits the program if there's an error
 */
void generateProgramAsm(CompilerOptions *compilerOptions, Program *program, int offset, HashTable *parentVariables, HashTable *globalVariables);
void generateAsm(CompilerOptions *compilerOptions);
void compile(CompilerOptions *compilerOptions);
void runProgram(CompilerOptions *compilerOptions);

#endif
