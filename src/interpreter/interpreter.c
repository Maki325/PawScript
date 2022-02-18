#include "interpreter.h"

void *interpretBinaryOperation(Token *token, HashTable *table, const char *error) {
  BinaryOperationValue *value = (BinaryOperationValue*) token->data;
  Token *leftToken = value->operandOne, *rightToken = value->operandTwo;
  if(!leftToken || !rightToken) {
    snprintf(
      error, 512,
      "%s:%zu:%zu: Add token doesn't have 2 params!",
      token->file, token->line, token->column
    );
    return false;
  }
  uint32_t left, right;
  switch (leftToken->type) {
  case TOKEN_VALUE:
    left = *((uint32_t*) leftToken->data);
    break;
  case TOKEN_NAME:
    left = *((uint32_t*) getElementFromHashTable(table, (const char*) leftToken->data));
    break;
  default:
    left = *((uint32_t*) interpretBinaryOperation(leftToken, table, error));
    break;
  }
  switch (rightToken->type) {
  case TOKEN_VALUE:
    right = *((uint32_t*) rightToken->data);
    break;
  case TOKEN_NAME:
    right = *((uint32_t*) getElementFromHashTable(table, (const char*) rightToken->data));
    break;
  default:
    right = *((uint32_t*) interpretBinaryOperation(rightToken, table, error));
    break;
  }

  uint32_t *sum = malloc(sizeof(uint32_t));

  switch (token->type) {
  case TOKEN_ADD: {
    (*sum) = left + right;
    return sum;
  }
  case TOKEN_SUBTRACT: {
    (*sum) = left - right;
    return sum;
  }
  default:
    break;
  }
}

bool interpretToken(Program *program, size_t i, HashTable *table, char *name, char **namePtr, char *error) {
  Token *token = program->instructions[i];
  switch(token->type) {
    case TOKEN_TYPE: {
      // Since we don't allocate memory in simulate
      // We don't need this type
      break;
    }
    case TOKEN_NAME: {
      char *mName = token->data;
      if(i == 0) {
        snprintf(
          error, 512,
          "%s:%zu:%zu: No type for variable `%s`!",
          token->file, token->line, token->column,
          mName
        );
        return false;
      }
      switch (program->instructions[i - 1]->type) {
      case TOKEN_ASSIGN:
        if(strcmp(mName, name) == 0) {
          snprintf(
            error, 512,
            "%s:%zu:%zu: Can't assign a variable `%s` to itself!",
            token->file, token->line, token->column,
            mName
          );
          return false;
        } else if(!existsElementInHashTable(table, mName)) {
          snprintf(
            error, 512,
            "%s:%zu:%zu: Can't assign undeclared variable `%s`!",
            token->file, token->line, token->column,
            mName
          );
          return false;
        }
        void *value = getElementFromHashTable(table, mName);
        setElementInHashTable(table, name, value);
        break;
      case TOKEN_TYPE:
        (*namePtr) = mName;
        setElementInHashTable(table, mName, NULL);
        break;
      default:
        break;
      }
      break;
    }
    case TOKEN_ASSIGN: {
      if(name == NULL) {
        snprintf(
          error, 512,
          "%s:%zu:%zu: Trying to assign to no variable! %d",
          token->file, token->line, token->column,
          i
        );
        return false;
      }
      break;
    }
    case TOKEN_VALUE: {
      setElementInHashTable(table, name, token->data);
      (*namePtr) = NULL;
      break;
    }
    case TOKEN_PRINT: {
      printf("%d\n", *(int*)getElementFromHashTable(table, token->data));
      break;
    }
    case TOKEN_SEMICOLON: {
      (*namePtr) = NULL;
      break;
    }
    case TOKEN_ADD:
    case TOKEN_SUBTRACT: {
      setElementInHashTable(table, name, interpretBinaryOperation(token, table, error));
      (*namePtr) = NULL;
      break;
    }
    default: {
      ASSERT(false, "Not all operations are implemented in interpret!");
    }
  }
  return true;
}

void interpret(Program *program, char *error) {
  ASSERT(TOKEN_COUNT == 8, "Not all operations are implemented in interpret!");
  char *name = NULL;
  HashTable *table = createHashTable(255);
  for(size_t i = 0;i < program->count;i++) {
    if(!interpretToken(program, i, table, name, &name, error)) {
      return;
    }
  }
}