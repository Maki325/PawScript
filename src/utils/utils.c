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
      printf("%*s - name: %s (%p)\n", funcDepth, "", functionName, data);
      printf("%*s - return type: %s\n", funcDepth, "", getTypeName(data->functionType->output));
      
      printf("%*s - parameters: %p, count: %zu\n", funcDepth, "", data->parameters, data->parameters->count);
      for(size_t i = 0;i < data->parameters->count;i++) {
        printToken(data->parameters->instructions[i], depth + 2 * TAB_SPACES, i);
      }
      printf("%*s - body: %p\n", funcDepth, "", data->body);
      printProgram(data->body, funcDepth + 2 * TAB_SPACES);
    }
  }
  printf("%*s - offset: %" PRIi32 "\n", depth, "", program->variableOffset);
  printf("%*s - instructions (%zu):\n", depth, "", program->count);
  for(size_t i = 0; i < program->count;i++) {
    Token *token = program->instructions[i];
    printToken(token, depth + 1 * TAB_SPACES, i);
  }
}
void printToken(Token *token, unsigned int depth, size_t index) {
  ASSERT(TOKEN_COUNT == 31, "Not all operations are implemented in createTokenFromString!");
  ASSERT(BASIC_TYPES_COUNT == 7, "Not all types are implemented in printToken!");

  printf("%*s - ", depth, "");
  printf("[%02zu]: ", index);
  switch(token->type) {
    case TOKEN_TYPE: {
      printf("TYPE: %s\n", getTypeName(*((Type*) token->data)));
      break;
    }
    case TOKEN_NAME: {
      NameData *value = token->data;
      if(!value->type) {
        printf("NAME: %s, CODE NAME: %s, TYPE: (NULL, NULL), MUTABLE: %d, OFFSET: %" PRIi32 "\n", value->variableName, value->name, value->mutable, value->offset != NULL ? *value->offset : 0);
      } else {
        printf("NAME: %s, CODE NAME: %s, TYPE: (%p, %s), MUTABLE: %d, OFFSET: %" PRIi32 "\n", value->variableName, value->name, value->type, getTypeName(*((Type *) value->type)), value->mutable, value->offset != NULL ? *value->offset : 0);
      }
      break;
    }
    case TOKEN_INDEX: {
      IndexData *data = token->data;
      NameData *value = data->nameData;
      if(!value->type) {
        printf("INDEX NAME: %s, INDEX: %zu, CODE NAME: %s, TYPE: (NULL, NULL), MUTABLE: %d, OFFSET: %" PRIi32 "\n", value->variableName, data->index, value->name, value->mutable, value->offset != NULL ? *value->offset : 0);
      } else {
        printf("INDEX NAME: %s, INDEX: %zu, CODE NAME: %s, TYPE: (%p, %s), MUTABLE: %d, OFFSET: %" PRIi32 "\n", value->variableName, data->index, value->name, value->type, getTypeName(*((Type *) value->type)), value->mutable, value->offset != NULL ? *value->offset : 0);
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
    case TOKEN_BRACKETS: {
      TokenPriorityData *priorityData = token->data;
      printf("BRACKETS: %p, count: %zu\n", priorityData, priorityData->count);
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
      printf("IF: %p, {next: %zu, end: %zu}\n", data, data->nextInstruction, data->endInstruction);

      printf("%*s Condition:\n", depth + 1 * TAB_SPACES + 1, "");
      printToken(data->condition, depth + 2 * TAB_SPACES, 0);
      printProgram(data->program, depth + 2 * TAB_SPACES);
      break;
    }
    case TOKEN_ELSE: {
      ControlFlowBlock *data = token->data;
      printf("ELSE: %p, {next: %zu, end: %zu}\n", data, data->nextInstruction, data->endInstruction);

      printProgram(data->program, depth + 2 * TAB_SPACES);
      break;
    }
    case TOKEN_VALUE: {
      printf("VALUE: {type: ");
      ValueData *data = token->data;
      switch (data->type.basicType) {
        case BASIC_TYPE_BOOL: {
          printf("BOOL, value: %s}\n", getBoolValue(data->data) == 0 ? "FALSE" : "TRUE");
          break;
        }
        case BASIC_TYPE_INT: {
          printf("INT, value: %" PRIu64 "}\n", getIntValue(data->data));
          break;
        }
        case BASIC_TYPE_CHAR: {
          printf("CHAR, value: %" PRIu32 "}\n", getCharValue(data->data));
          break;
        }
        case BASIC_TYPE_VOID: {
          printf("VOID}\n");
          break;
        }
        case BASIC_TYPE_FUNCTION: {
          printf("FUNCTION}\n");
          break;
        }
        case BASIC_TYPE_NONE: {
          printf("NONE}\n");
          break;
        }
        case BASIC_TYPES_COUNT: {
          printf("COUNT}\n");
          break;
        }
        case BASIC_TYPE_ARRAY: {
          // ASSERT(false, "TODO!");
          printf("ARRAY}\n");
          break;
        }
      }
      break;
    }
    case TOKEN_RETURN: {
      TokenPriorityData *priorityData = token->data;
      printf("RETURN: %p", priorityData);
      if(!priorityData) {
        putc('\n', stdout);
        break;
      }
      printf(", count: %zu\n", priorityData->count);
      for(size_t i = 0;i < priorityData->count;i++) {
        printToken(priorityData->instructions[i], depth + 1 * TAB_SPACES, i);
      }
      break;
    }
    case TOKEN_PRINT: {
      printf("PRINT %p\n", token->data);
      if(token->data) {
        printToken(token->data, depth + 1 * TAB_SPACES, 0);
      }
      break;
    }
    case TOKEN_FUNCTION_CALL: {
      FunctionCallData *data = token->data;
      TokenPriorityData *priorityData = data->arguments;
      printf("FUNCTION CALL: %p \"%s\", count: %zu\n", data->function, data->function ? data->function->name : "NULL", priorityData->count);
      for(size_t i = 0;i < priorityData->count;i++) {
        printToken(priorityData->instructions[i], depth + 1 * TAB_SPACES, i);
      }
      break;
    }
    case TOKEN_ADD:
      printf("ADD: %p\n", token->data);
      goto printToken_binaryOperation;
    case TOKEN_SUBTRACT:
      printf("SUBTRACT: %p\n", token->data);
      goto printToken_binaryOperation;
    case TOKEN_GREATER_THAN:
      printf("GREATER THAN: %p\n", token->data);
      goto printToken_binaryOperation;
    case TOKEN_LESS_THAN:
      printf("LESS THAN: %p\n", token->data);
      goto printToken_binaryOperation;
    case TOKEN_EQUALS:
      printf("EQUALS: %p\n", token->data);
      goto printToken_binaryOperation;
    case TOKEN_NOT_EQUALS:
      printf("NOT EQUALS: %p\n", token->data);
    printToken_binaryOperation: {

      BinaryOperationData *data = token->data;
      if(!data) {
        break;
      }
      printToken(data->operandOne, depth + 1 * TAB_SPACES, 0);
      printToken(data->operandTwo, depth + 1 * TAB_SPACES, 1);
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

const char *getBoolStringFromValue(uint8_t *value) {
  return *value == 0 ? "false" : "true";
}

uint8_t getNormalizedBoolValueFromUInt64(uint64_t *value) {
  return *value != 0;
}
uint8_t getNormalizedBoolValueFromUInt32(uint32_t *value) {
  return *value != 0;
}
uint8_t getNormalizedBoolValueFromUInt8(uint8_t *value) {
  return *value != 0;
}

uint32_t getNormalizedCharValueFromUInt64(uint64_t *value) {
  return (uint32_t) *value;
}

const char *getSign(int32_t value) {
  return value >= 0 ? "+" : "-";
}

uint64_t getIntValue(void *data) {
  return *((uint64_t*) data);
}
uint8_t getBoolValue(void *data) {
  return *((uint8_t*) data);
}
uint32_t getCharValue(void *data) {
  return *((uint32_t*) data);
}

const char *getFunctionNameFromCall(FunctionCallData *data) {
  if(data->function) return data->function->name;
  return data->nameData->variableName;
}

Type getFunctionReturnTypeFromCall(FunctionCallData *data) {
  FunctionType *ft =
    data->function ? data->function->functionType : data->nameData->type->data;
  return ft->output;
}

const char *getBasicTypeName(BasicType type) {
  ASSERT(BASIC_TYPES_COUNT == 7, "Not all types are implemented in getBasicTypeName!");
  switch (type) {
    case BASIC_TYPE_INT:      return "int";
    case BASIC_TYPE_BOOL:     return "bool";
    case BASIC_TYPE_VOID:     return "void";
    case BASIC_TYPE_CHAR:     return "char";
    case BASIC_TYPE_FUNCTION: return "function";
    case BASIC_TYPE_ARRAY: return "array";
    // TODO: Create a better function type such thats it can have actuall input and output types!
    case BASIC_TYPE_NONE:     return "NONE!!!";
    default:            return "Unknown Token!!!";
  }
}

const char *getTypeName(Type type) {
  ASSERT(BASIC_TYPES_COUNT == 7, "Not all types are implemented in getTypeName!");
  switch (type.basicType) {
    case BASIC_TYPE_INT:
    case BASIC_TYPE_BOOL:
    case BASIC_TYPE_CHAR:
    case BASIC_TYPE_VOID: return getBasicTypeName(type.basicType);
    case BASIC_TYPE_FUNCTION: {
      // TODO
    }
    case BASIC_TYPE_ARRAY: {
      // TODO
    }

    default: return getBasicTypeName(type.basicType);
  }
}

/// Mask of the value bits of a continuation byte.
const uint8_t CONT_MASK = 0b00111111;

// Line by line replica of `next_code_point` from the rust compiler
// Path: library/core/src/str/validations.rs
uint32_t turnCharsIntoCodePoint(const char *chars, size_t *length) {
  if(*length == 0) {
    return 0;
  }
  uint8_t x = chars[0];
  --(*length);

  if((int) x < 128) return (uint32_t) x;
  if(*length == 0) {
    return 0;
  }

  // Multibyte case follows
  // Decode from a byte combination out of: [[[x y] z] w]
  // NOTE: Performance is sensitive to the exact formulation here
  uint32_t init = utf8_first_byte(x, 2);

  uint8_t y = chars[1];
  --(*length);

  uint32_t ch = utf8_acc_cont_byte(init, y);
  if(x >= 0xE0) {
    // [[x y z] w] case
    // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
    // SAFETY: `bytes` produces an UTF-8-like string,
    // so the iterator must produce a value here.
    if(*length == 0) {
      return 0;
    }
    uint8_t z = chars[2];
    --(*length);

    uint32_t y_z = utf8_acc_cont_byte((y & CONT_MASK), z);
    ch = init << 12 | y_z;
    
    if(x >= 0xF0) {
      // [x y z w] case
      // use only the lower 3 bits of `init`
      // SAFETY: `bytes` produces an UTF-8-like string,
      // so the iterator must produce a value here.
      if(*length == 0) {
        return 0;
      }
      uint8_t w = chars[3];
      --(*length);

      ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
    }
  }

  return ch;
}

FILE *openFile(const char *filePath, const char *modes) {
  FILE *file = fopen(filePath, modes);
  if(!file) {
    fprintf(stderr, "Couldn't open file \"%s\": %s\n", filePath, strerror(errno));
    exit(ERROR_OPEN_FILE);
  }
  return file;
}
