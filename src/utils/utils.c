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
  for(size_t i = 0;i < len - 1;i++) {
    basename[i] = path[i + slash];
  }
  return basename;
}
char *getBasenameWithDirectory(char *path) {
  size_t dot = 0, len = strlen(path);
  for(int i = len - 1;i >= 0;i--) {
    if(dot == 0 && path[i] == '.') {
      dot = i;
    } else if(path[i] == '/' || path[i] == '\\') {
      break;
    }
  }
  len = (dot == 0 ? len : dot) + 1;
  char *basename = calloc(len, sizeof(char));
  for(size_t i = 0;i < len - 1;i++) {
    basename[i] = path[i];
  }
  return basename;
}
bool isDigit(char c) {
  return c >= '0' && c <= '9';
}
void printProgram(Program *program) {
  printf("Program: %p\n", program);
  printf(" - count: %zu\n", program->count);
  printf(" - capacity: %zu\n", program->capacity);
  printf(" - instructions:\n");
  for(size_t i = 0; i < program->count;i++) {
    Token *token = program->instructions[i];
    printf("  - ");
    printToken(token, 0, i);
  }
}
void printToken(Token *token, size_t depth, size_t index) {
  ASSERT(TOKEN_COUNT == 18, "Not all operations are implemented in createTokenFromString!");
  
  printf("[%02zu]: ", index);
  switch(token->type) {
    case TOKEN_TYPE: {
      printf("TYPE: %zu\n", (size_t) token->data);
      break;
    }
    case TOKEN_NAME: {
      NameValue *value = token->data;
      if(!value->type) {
        printf("NAME: %s, TYPE: (NULL)\n", value->variableName);
      } else {
        printf("NAME: %s, TYPE: (%p, %s)\n", value->variableName, value->type, getTypeName(*((Type *) value->type)));
      }
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
      ValueData *value = token->data;
      printf("VALUE: (type: %s, value: ", getTypeName(value->type));
      switch (value->type) {
        case TYPE_INT:
          printf("%" PRIu32, *((uint32_t*)value->data));
          break;
        case TYPE_BOOL:
          printf("%d", *((uint8_t*)value->data));
          break;
        case TYPE_NONE:
        case TYPES_COUNT:
          ASSERT(false, "Unreachable!");
          break;
        default:
          break;
      }
      printf(")\n");
      break;
    }
    case TOKEN_PRINT: {
      printf("PRINT: %s\n", (char*) token->data);
      break;
    }
    case TOKEN_PARENTHESES_OPEN: {
      printf("(\n");
      break;
    }
    case TOKEN_PARENTHESES_CLOSE: {
      printf(")\n");
      break;
    }
    case TOKEN_BRACES_OPEN: {
      printf("{\n");
      break;
    }
    case TOKEN_BRACES_CLOSE: {
      printf("}\n");
      break;
    }
    case TOKEN_PRIORITY: {
      printf("PRIORITY\n");
      TokenPriorityValue *value = token->data;
      for(size_t i = 0; i < value->count;i++) {
        printf("\t- ");
        printToken(value->instructions[i], depth + 1, i);
      }
      break;
    }
    case TOKEN_SCOPE: {
      printf("SCOPE: ");
      Program *value = token->data;
      if(value->instructions == NULL) printf("Undefined scope!\n");
      else {
        printf("%p\n", value);
        for(size_t i = 0; i < value->count;i++) {
          printf("\t- ");
          printToken(value->instructions[i], depth + 1, i);
        }
      }
      break;
    }
    case TOKEN_ADD:
    case TOKEN_SUBTRACT: {
      if(token->type == TOKEN_ADD) printf("ADD\n");
      else printf("SUBTRACT\n");
  
      BinaryOperationValue *value = (BinaryOperationValue*) token->data;
      if(!value) break;
      Token *leftToken = value->operandOne, *rightToken = value->operandTwo;

      printf("  - Left: ");
      if(leftToken)
        printToken(leftToken, depth + 1, index);
      else
        printf("(NULL)\n");

      printf("  - Right: ");
      if(rightToken)
        printToken(rightToken, depth + 1, index);
      else
        printf("(NULL)\n");
      break;
    }
    case TOKEN_GREATER_THAN:
    case TOKEN_LESS_THAN: {
      if(token->type == TOKEN_GREATER_THAN) printf("GREATER\n");
      else printf("LESS\n");
      BinaryOperationValue *value = (BinaryOperationValue*) token->data;
      if(!value) break;
      Token *leftToken = value->operandOne, *rightToken = value->operandTwo;

      printf("  - Left: ");
      if(leftToken)
        printToken(leftToken, depth + 1, index);
      else
        printf("(NULL)\n");

      printf("  - Right: ");
      if(rightToken)
        printToken(rightToken, depth + 1, index);
      else
        printf("(NULL)\n");
      break;
    }
    case TOKEN_IF: {
      printf("IF");
      ControlFlowBlock *value = token->data;
      if(!value) {
        printf("\n");
        break;
      }
      printf(" {next: %zu, end: %zu}\n", value->nextInstruction, value->endInstruction);
      Token *condition = value->condition;
      printf("Condition: ");
      printToken(condition, depth+1, 0);

      Program *p = value->program;
      printf("%p\n", p);
      if(!p) break;
      for(size_t i = 0; i < p->count;i++) {
        printf("\t- ");
        printToken(p->instructions[i], depth + 1, i);
      }

      break;
    }
    case TOKEN_ELSE: {
      printf("ELSE");
      ControlFlowBlock *value = token->data;
      if(!value) {
        printf("\n");
        break;
      }
      printf(" {next: %zu, end: %zu}\n", value->nextInstruction, value->endInstruction);

      Program *p = value->program;
      printf("%p\n", p);
      if(!p) break;
      for(size_t i = 0; i < p->count;i++) {
        printf("\t- ");
        printToken(p->instructions[i], depth + 1, i);
      }

      break;
    }
    default: {
      printf("UNKNOWN TOKEN (Type id: %d)!!!\n", token->type);
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

int trimLeft(char **text, size_t *length) {
  size_t i = 0;
  while(isspace((*text)[0]) && *length) {
    (*text)++;
    (*length)--;
    i++;
  }
  return i;
}
int trimRight(const char *text, size_t *length) {
  if(*length == 0) return 0;
  size_t i = 0;
  while(isspace(text[*length - 1]) && *length) {
    (*length)--;
    i++;
  }
  return i;
}

int rstrncmp(const char *a, size_t aLength, const char *b, size_t bLength, size_t length) {
  while(length > 0 && aLength > 0 && bLength > 0) {
    if(a[--aLength] != b[--bLength]) return a - b;
    length--;
  }
  if(length == 0) return 0;
  return a - b;
}

void printTokenLocation(Token *token, FILE *out) {
  fprintf(out, "%s:%zu:%zu", token->file, token->line, token->column);
}

int strnint(const char *str, size_t n) {
  int out = 0;
  for(size_t i = 0;i < n;i++) {
    out = out * 10 + str[i] - '0';
  }
  return out;
}
uint32_t strnuint32(const char *str, size_t n) {
  uint32_t out = 0;
  for(size_t i = 0;i < n;i++) {
    out = out * 10 + str[i] - '0';
  }
  return out;
}

FILE *openFile(const char *filePath, const char *modes) {
  FILE *file = fopen(filePath, modes);
  if(!file) {
    fprintf(stderr, "Couldn't open file \"%s\": %s\n", filePath, strerror(errno));
    exit(1);
  }
  return file;
}
