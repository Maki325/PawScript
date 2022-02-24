#ifndef TOKENIZER_H
#define TOKENIZER_H
#include "includes.h"
#include "utils/hashtable.h"

typedef enum TokenType {
  TOKEN_NAME = 0,
  TOKEN_SEMICOLON,
  TOKEN_ASSIGN,
  TOKEN_VALUE,
  TOKEN_PRINT,
  TOKEN_ADD,
  TOKEN_SUBTRACT,
  TOKEN_GREATER_THAN,
  TOKEN_LESS_THAN,
  TOKEN_COUNT
} TokenType;

typedef enum Type {
  TYPE_NONE = 0,
  TYPE_INT,
  TYPES_COUNT
} Type;

typedef struct Token
{
  TokenType type;
  void *data;

  const char *file;

  // Starts at 1
  size_t line;
  size_t column;
} Token;

typedef struct NameValue {
  char *name;
  Type *type;
  bool assignType;
} NameValue;

typedef struct BinaryOperationValue {
  Token *operandOne;
  Token *operandTwo;
} BinaryOperationValue;

Token *createToken(Token *create);

typedef struct Program {
  Token **instructions;
  size_t count;
  size_t capacity;

  HashTable *variableTypes;
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
void deleteProgram(Program *program);
void expandProgramInstructions(Program *program);
void pushProgramInstruction(Program *program, Token *instruction);
Token *popProgramInstruction(Program *program);
Token *getProgramInstruction(Program *program, size_t i);
Type *getTokenType(Program *program, Token *token);

// Returns NULL if there are no errors
// Otherwise returns the Token that provides the error
Token *checkProgram(Program *program);

Program *createProgramFromFile(const char *filePath, char *error);


#endif
