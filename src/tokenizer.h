#ifndef TOKENIZER_H
#define TOKENIZER_H
#include "includes.h"
#include "utils/hashtable.h"

typedef enum TokenType {
  TOKEN_TYPE = 0,
  TOKEN_NAME,
  TOKEN_SEMICOLON,
  TOKEN_ASSIGN,
  TOKEN_VALUE,
  TOKEN_PRINT,
  TOKEN_ADD,
  TOKEN_SUBTRACT,
  TOKEN_COUNT
} TokenType;

typedef enum Type {
  TYPE_INT = 0,
  TYPES_COUNT
} Type;

typedef struct Token
{
  TokenType type;
  void *data;

  const char *file;
  size_t line;
  size_t column;
} Token;

typedef struct BinaryOperationValue {
  Token *operandOne;
  Token *operandTwo;
} BinaryOperationValue;

Token *createToken(Token *create);

typedef struct Program {
  Token **instructions;
  size_t count;
  size_t capacity;
} Program;

typedef struct CreateTokenFromString {
  Token *last;
  char *string;
  size_t length;

  Program *program;
  char *file;
  size_t line;
  size_t column;
  const char *error;
} CreateTokenFromString;
Token *createTokenFromString(CreateTokenFromString *createTokenFromString);

Program *createProgram();
void expandProgramInstructions(Program *program);
void pushProgramInstruction(Program *program, Token *instruction);
Token *popProgramInstruction(Program *program);

Program *createProgramFromFile(const char *filePath, char *error);


#endif
