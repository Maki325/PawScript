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

/**
 * @brief Generated the asm code of the operation
 * 
 * @param compilerOptions The pointer to CompilerOptions struct
 * @param operationToken The pointer to the Token with one of the binary operations types (See @link generateAssignAsm test
 */
void generateBinaryOperationAsm(CompilerOptions *compilerOptions, Token *operationToken);

/**
 * @brief Generates the asm code of the assign token
 * 
 * @param compilerOptions The pointer to CompilerOptions struct
 * @param data The pointer to the data of the variable we want to assign to
 * @param program The pointer to the program of the variable
 * @param i Pointer to the index variable that's used in the parent function
 */
void generateAssignAsm(CompilerOptions *compilerOptions, NameData *data, Program *program, size_t *i);

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
