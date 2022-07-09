#include "utils.h"
#include "pawscript_error.h"

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
  ASSERT(TOKEN_COUNT == 26, "Not all operations are implemented in createTokenFromString!");
  
  printf("[%02zu]: ", index);
  switch(token->type) {
    case TOKEN_TYPE: {
      printf("TYPE: %s\n", getTypeName((Type) token->data));
      break;
    }
    case TOKEN_NAME: {
      NameData *value = token->data;
      if(!value->type) {
        printf("NAME: %s, TYPE: (NULL, NULL), ASSIGN: %d\n", value->variableName, value->assignType);
      } else {
        printf("NAME: %s, TYPE: (%p, %s), ASSIGN: %d\n", value->variableName, value->type, getTypeName(*((Type *) value->type)), value->assignType);
      }
      break;
    }
    default: {
      printf("%s\n", getTokenTypeName(token->type));
      // printf("UNKNOWN TOKEN (Type id: %d)!!!\n", token->type);
      // ASSERT(false, "Not all operations are implemented in printToken!");
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
    exit(ERROR_OPEN_FILE);
  }
  return file;
}
