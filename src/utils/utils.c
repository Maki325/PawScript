#include "utils.h"

char *popArgument(Args *args) {
  char *arg = args->args[0];
  args->args++;
  args->count--;
  return arg;
}
char *getBasename(char *path) {
  size_t dot = 0, slash = 0, len = strlen(path);
  for(int i = len - 1;i >= 0;i--) {
    if(dot == 0 && path[i] == '.') {
      dot = i;
    } else if(path[i] == '/' || path[i] == '\\') {
      slash = i + 1;
      break;
    }
  }
  len = (dot == 0 ? len : dot) - slash + 1;
  char *basename = calloc(len, sizeof(char));
  for(int i = 0;i < len - 1;i++) {
    basename[i] = path[i + slash];
  }
  return basename;
}
char *getBasenameWithDirectory(char *path) {
  size_t dot = 0, slash = 0, len = strlen(path);
  for(int i = len - 1;i >= 0;i--) {
    if(dot == 0 && path[i] == '.') {
      dot = i;
    } else if(path[i] == '/' || path[i] == '\\') {
      slash = i + 1;
      break;
    }
  }
  len = (dot == 0 ? len : dot) + 1;
  char *basename = calloc(len, sizeof(char));
  for(int i = 0;i < len - 1;i++) {
    basename[i] = path[i];
  }
  return basename;
}
bool isDigit(char c) {
  return c >= '0' && c <= '9';
}
int strnint(const char *str, size_t n) {
  int out = 0;
  for(size_t i = 0;i < n;i++) {
    out = out * 10 + str[i] - '0';
  }
  return out;
}
void printProgram(Program *program) {
  printf("Program: %p\n", program);
  printf(" - count: %zu\n", program->count);
  printf(" - capacity: %zu\n", program->capacity);
  printf(" - instructions:\n");
  for(size_t i = 0; i < program->count;i++) {
    Token *token = program->instructions[i];
    printf("  - ");
    printToken(token);
  }
}
void printToken(Token *token) {
  bool isAdd = false;
  switch(token->type) {
    case TOKEN_TYPE: {
      printf("TYPE: ");
      Type type = (Type) token->data;
      switch(type) {
        case TYPE_INT: {
          printf("INT");
          break;
        }
        default: {
          printf("UNKNOWN");
          break;
        }
      }
      printf("\n");
      break;
    }
    case TOKEN_NAME: {
      printf("NAME: %s\n", token->data);
      break;
    }
    case TOKEN_SEMICOLON: {
      printf("SEMICOLON\n");
      break;
    }
    case TOKEN_ASSIGN: {
      printf("ASSIGN\n");
      break;
    }
    case TOKEN_VALUE: {
      printf("VALUE: %d\n", *((int*)token->data));
      break;
    }
    case TOKEN_PRINT: {
      printf("PRINT: %s\n", token->data);
      break;
    }
    case TOKEN_ADD:
      isAdd = true;
    case TOKEN_SUBTRACT: {
      BinaryOperationValue *value = (BinaryOperationValue*) token->data;
      Token *leftToken = value->operandOne, *rightToken = value->operandTwo;
  
      if(isAdd) printf("ADD\n");
      else printf("SUBTRACT\n");
      printf("  - Left: ");
      printToken(leftToken);

      printf("  - Right: ");
      printToken(rightToken);
      break;
    }
    default: {
      printf("UNKNOWN!!!\n");
      break;
    }
  }
}
void printn(const char* string, size_t length) {
  for(size_t i = 0;i < length;i++) {
    printf("%c", string[i]);
  }
  printf("\n");
}