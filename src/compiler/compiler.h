#ifndef PS_COMPILER_H
#define PS_COMPILER_H
#include "../includes.h"
#include "../tokenizer/tokenizer.h"

typedef struct CompilerOptions {
  Program *program;
  FILE *output;
  bool silent;
  const char *basename;
} CompilerOptions;

typedef enum Register {
  REGISTER_A = 0,
  REGISTER_B,
  REGISTER_C,
  REGISTER_D,
  REGISTER_SI,
  REGISTER_DI,
  REGISTER_BP,
  REGISTER_SP,
  REGISTER_8,
  REGISTER_9,
  REGISTER_10,
  REGISTER_11,
  REGISTER_12,
  REGISTER_13,
  REGISTER_14,
  REGISTER_15,
  REGISTER_COUNT
} Register;

const char *get64BitRegister(Register reg);
const char *get32BitRegister(Register reg);

const char *getRegisterBySize(Register reg, Type type);

char *getInitializedType(Type type);
char *getUninitializedType(Type type);

void addPrintFunction(FILE *out);
void addCharPrintFunction(FILE *out);
void prepareFileForCompile(FILE *out);
void postCompile(CompilerOptions *compilerOptions);

/**
 * @brief Generated the asm code of the operation
 * 
 * @param compilerOptions The pointer to CompilerOptions struct
 * @param operationToken The pointer to the Token with one of the binary operations types (See isOperationTokenType)
 */
void generateBinaryOperationAsm(CompilerOptions *compilerOptions, Program *program, Token *operationToken);

/**
 * @brief Generates the asm code of the assign token
 * 
 * @param compilerOptions The pointer to CompilerOptions struct
 * @param data The pointer to the data of the variable we want to assign to
 * @param program The pointer to the program of the variable
 * @param i Pointer to the index variable that's used in the parent function
 */
void generateAssignAsm(CompilerOptions *compilerOptions, NameData *data, Program *program, size_t *i);

void generateValueAsm(CompilerOptions *compilerOptions, Token *token, Register destination);
void generateNameAsm(CompilerOptions *compilerOptions, Program *program, Token *token, Register destination);

void generateFunctionCallAsm(CompilerOptions *compilerOptions, Program *program, Token *token);

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
void generateFunctionAsm(CompilerOptions *compilerOptions, FunctionDefinition *functionData);

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
void generateProgramAsm(CompilerOptions *compilerOptions, Program *program);
void generateAsm(CompilerOptions *compilerOptions);
void compile(CompilerOptions *compilerOptions);
void runProgram(CompilerOptions *compilerOptions);

#endif
