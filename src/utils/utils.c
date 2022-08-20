#include "utils.h"
#include "../pawscript_error.h"

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
void printProgram(Program *program, unsigned int depth) {
  printf("%*sProgram: %p\n", depth, "", program);
  printf("%*s - count: %zu\n", depth, "", program->count);
  printf("%*s - capacity: %zu\n", depth, "", program->capacity);
  if(program->functions && program->functions->size) {
    printf("%*s - functions:\n", depth, "");
    unsigned int funcDepth = depth + 1 * TAB_SPACES;
    for(size_t i = 0; i < program->functions->capacity;i++) {
      const char *functionName = program->functions->elements[i].key;
      if(!functionName) continue;
      FunctionDefinition *data = program->functions->elements[i].value;
      printf("%*s - name: %s\n", funcDepth, "", functionName);
      printf("%*s - return type: %s\n", funcDepth, "", getTypeName(data->returnType));
      
      printf("%*s - parameters: %p, count: %zu\n", funcDepth, "", data->parameters, data->parameters->count);
      for(size_t i = 0;i < data->parameters->count;i++) {
        printToken(data->parameters->instructions[i], depth + 2 * TAB_SPACES, i);
      }
      printf("%*s - body: %p\n", funcDepth, "", data->body);
      printProgram(data->body, funcDepth + 2 * TAB_SPACES);
    }
  }
  printf("%*s - instructions:\n", depth, "");
  for(size_t i = 0; i < program->count;i++) {
    Token *token = program->instructions[i];
    printToken(token, depth + 1 * TAB_SPACES, i);
  }
}
void printToken(Token *token, unsigned int depth, size_t index) {
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in createTokenFromString!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in printToken!");

  
  printf("%*s - ", depth, "");
  printf("[%02zu]: ", index);
  switch(token->type) {
    case TOKEN_TYPE: {
      printf("TYPE: %s\n", getTypeName((Type) token->data));
      break;
    }
    case TOKEN_NAME: {
      NameData *value = token->data;
      if(!value->type) {
        printf("NAME: %s, TYPE: (NULL, NULL), MUTABLE: %d\n", value->variableName, value->mutable);
      } else {
        printf("NAME: %s, TYPE: (%p, %s), MUTABLE: %d\n", value->variableName, value->type, getTypeName(*((Type *) value->type)), value->mutable);
      }
      break;
    }
    case TOKEN_DECLARE_FUNCTION: {
      printf("TOKEN_DECLARE_FUNCTION: %p\n", token->data);
      break;
    }
    case TOKEN_PRIORITY: {
      TokenPriorityData *priorityData = token->data;
      printf("PRIORITY: %p, count: %zu\n", priorityData, priorityData->count);
      for(size_t i = 0;i < priorityData->count;i++) {
        printToken(priorityData->instructions[i], depth + 1 * TAB_SPACES, i);
      }
      break;
    }
    case TOKEN_SCOPE: {
      Program *scopeProgram = token->data;
      printf("SCOPE: %p\n", scopeProgram);
      printProgram(scopeProgram, depth + 2 * TAB_SPACES);
      break;
    }
    case TOKEN_IF: {
      ControlFlowBlock *data = token->data;
      printf("IF: %p\n", data);
      
      printToken(data->condition, depth + 1 * TAB_SPACES, 0);
      printProgram(data->program, depth + 2 * TAB_SPACES);
      break;
    }
    case TOKEN_VALUE: {
      printf("VALUE: {type: ");
      ValueData *data = token->data;
      switch (data->type) {
        case TYPE_BOOL: {
          printf("BOOL, value: %s}\n", data->data == 0 ? "FALSE" : "TRUE");
          break;
        }
        case TYPE_INT: {
          printf("INT, value: %" PRIu64 "}\n", *((uint64_t*) data->data));
          break;
        }
        case TYPE_VOID: {
          printf("VOID}\n");
          break;
        }
        case TYPE_FUNCTION: {
          printf("FUNCTION}\n");
          break;
        }
        case TYPE_NONE: {
          printf("NONE}\n");
          break;
        }
        case TYPES_COUNT: {
          printf("COUNT}\n");
          break;
        }
      }
      break;
    }
    default: {
      printf("%s\n", getTokenTypeName(token->type));
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
uint64_t strnuint64(const char *str, size_t n) {
  uint64_t out = 0;
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
