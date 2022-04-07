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
  TOKEN_GREATER_THAN,
  TOKEN_LESS_THAN,
  TOKEN_PARENTHESES_OPEN,   // (
  TOKEN_PARENTHESES_CLOSE,  // )
  TOKEN_PRIORITY,
  TOKEN_BRACES_OPEN,        // {
  TOKEN_BRACES_CLOSE,       // }
  TOKEN_SCOPE,
  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_EQUALS,
  TOKEN_NOT_EQUALS,
  TOKEN_FUNCTION,
  TOKEN_FUNCTION_CALL,
  TOKEN_COUNT
} TokenType;

static inline const char *getTokenTypeName(TokenType type) {
  ASSERT(TOKEN_COUNT == 20, "Not all tokens are implemented in getTokenTypeName!");
  switch (type) {
    case TOKEN_TYPE:              return "TOKEN_TYPE";
    case TOKEN_NAME:              return "TOKEN_NAME";
    case TOKEN_SEMICOLON:         return "TOKEN_SEMICOLON";
    case TOKEN_ASSIGN:            return "TOKEN_ASSIGN";
    case TOKEN_VALUE:             return "TOKEN_VALUE";
    case TOKEN_PRINT:             return "TOKEN_PRINT";
    case TOKEN_ADD:               return "TOKEN_ADD";
    case TOKEN_SUBTRACT:          return "TOKEN_SUBTRACT";
    case TOKEN_GREATER_THAN:      return "TOKEN_GREATER_THAN";
    case TOKEN_LESS_THAN:         return "TOKEN_LESS_THAN";
    case TOKEN_PARENTHESES_OPEN:  return "TOKEN_PARENTHESES_OPEN";
    case TOKEN_PARENTHESES_CLOSE: return "TOKEN_PARENTHESES_CLOSE";
    case TOKEN_PRIORITY:          return "TOKEN_PRIORITY";
    case TOKEN_BRACES_OPEN:       return "TOKEN_BRACES_OPEN";
    case TOKEN_BRACES_CLOSE:      return "TOKEN_BRACES_CLOSE";
    case TOKEN_SCOPE:             return "TOKEN_SCOPE";
    case TOKEN_COUNT:             return "TOKEN_COUNT";
    case TOKEN_IF:                return "TOKEN_IF";
    case TOKEN_ELSE:              return "TOKEN_ELSE";
    case TOKEN_EQUALS:            return "TOKEN_EQUALS";
    case TOKEN_NOT_EQUALS:        return "TOKEN_NOT_EQUALS";
    case TOKEN_FUNCTION:          return "TOKEN_FUNCTION";
    case TOKEN_FUNCTION_CALL:     return "TOKEN_FUNCTION_CALL";
    default:                      return "Unknown Token!!!";
  }
}

typedef enum Type {
  TYPE_NONE = 0,
  TYPE_INT,
  TYPE_BOOL,
  TYPES_COUNT
} Type;

static inline const char *getTypeName(Type type) {
  ASSERT(TYPES_COUNT == 3, "Not all types are implemented in getTypeName!");
  switch (type) {
    case TYPE_INT:  return "int";
    case TYPE_BOOL: return "bool";
    case TYPE_NONE: return "NONE!!!";
    default:        return "Unknown Token!!!";
  }
}

typedef struct Token
{
  TokenType type;
  void *data;

  const char *file;

  // Starts at 1
  size_t line;
  size_t column;
} Token;

typedef struct ValueData {
  Type type;
  void *data;
} ValueData;

typedef struct NameValue {
  const char *variableName;
  const char *name;
  Type *type;
  bool assignType;
} NameValue;

typedef struct BinaryOperationValue {
  Token *operandOne;
  Token *operandTwo;
} BinaryOperationValue;

typedef struct TokenPriorityValue {
  Token **instructions;
  size_t count;
} TokenPriorityValue;

extern size_t PROGRAM_COUNT;
typedef struct Program {
  size_t id;
  struct Program *parent;
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
  const char *file;
  size_t line;
  size_t column;
  char *error;
} CreateTokenFromString;

Token *createToken(Token *createToken);
Token *createTokenFromString(CreateTokenFromString *createTokenFromString);

Program *createProgram();
Program *createProgramWithParent(Program *parent);
void deleteProgram(Program *program);
void expandProgramInstructions(Program *program);
void pushProgramInstruction(Program *program, Token *instruction);
Token *popProgramInstruction(Program *program);
Token *getProgramInstruction(Program *program, size_t i, bool remove);

// Returns NULL if there are no errors
// Otherwise returns the Token that provides the error
Token *checkProgram(Program *program);

size_t isStringTokenFromRight(const char *string, size_t length);

typedef struct NameMapValue {
  Program *program;
  const char *name;
  Type *type;
} NameMapValue;

typedef struct ControlFlowBlock {
  Program *program;
  Token *condition;
  size_t nextInstruction;
  size_t endInstruction;
} ControlFlowBlock;

typedef struct FunctionData {
  TokenPriorityValue *inputs;
  Program *body;
} FunctionData;

bool isControlFlowBlock(TokenType type);

void checkInstruction(Program *program, Token *instruction);
int typesetProgram(Program *program);
void cleanupElseIfs(Program *program);
int crossrefrenceBlocks(Program *program);
int crossrefrenceVariables(Program *program, HashTable *parentNameMap);
int crossrefrenceOperations(Program *program);

Program *createProgramFromFile(const char *filePath, char *error);


#endif
