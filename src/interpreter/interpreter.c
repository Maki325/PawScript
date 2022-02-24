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
  NameValue *nameValue = NULL;
  uint32_t left, right;
  bool freeLeft = false, freeRight = false;
  switch (leftToken->type) {
    case TOKEN_VALUE:
      left = *((uint32_t*) leftToken->data);
      break;
    case TOKEN_NAME:
      nameValue = leftToken->data;
      left = *((uint32_t*) getElementFromHashTable(table, nameValue->name));
      break;
    default: {
      uint32_t *result = interpretBinaryOperation(leftToken, table, error);
      left = *(result);
      free(result);
      break;
    }
  }

  switch (rightToken->type) {
    case TOKEN_VALUE:
      right = *((uint32_t*) rightToken->data);
      break;
    case TOKEN_NAME:
      nameValue = rightToken->data;
      right = *((uint32_t*) getElementFromHashTable(table, nameValue->name));
      break;
    default: {
      uint32_t *result = interpretBinaryOperation(rightToken, table, error);
      right = *(result);
      free(result);
      break;
    }
  }

  uint32_t *sum = malloc(sizeof(uint32_t));

  switch (token->type) {
  case TOKEN_ADD: {
    (*sum) = left + right;
    break;
  }
  case TOKEN_SUBTRACT: {
    (*sum) = left - right;
    break;
  }
  case TOKEN_GREATER_THAN: {
    (*sum) = left > right;
    break;
  }
  case TOKEN_LESS_THAN: {
    (*sum) = left < right;
    break;
  }
  default:
    free(sum);
    sum = NULL;
    break;
  }

  return sum;
}

bool interpretToken(Program *program, size_t i, HashTable *table, char *name, char **namePtr, char *error) {
  Token *token = program->instructions[i];
  switch(token->type) {
    case TOKEN_NAME: {
      NameValue *value = token->data;
      char *mName = value->name;
      if(!value->type) {
        snprintf(
          error, 512,
          "%s:%zu:%zu: No type for variable `%s`!",
          token->file, token->line, token->column,
          mName
        );
        return false;
      }
      if(i == 0) {
        (*namePtr) = mName;
        setElementInHashTable(table, mName, NULL);
        break;
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
      case TOKEN_SEMICOLON:
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
    case TOKEN_SUBTRACT:
    case TOKEN_GREATER_THAN:
    case TOKEN_LESS_THAN: {
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
  ASSERT(TOKEN_COUNT == 9, "Not all operations are implemented in interpret!");
  char *name = NULL;
  HashTable *table = createHashTable(255);
  NameValue *nameValue = NULL;
  for(size_t i = 0;i < program->count;i++) {
    if(program->instructions[i]->type == TOKEN_NAME) {
      nameValue = program->instructions[i]->data;
    }
    if(!interpretToken(program, i, table, name, &name, error)) {
      return;
    }
  }
}