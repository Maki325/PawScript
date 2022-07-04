#include "tokenizer.h"
#include "utils/utils.h"

size_t PROGRAM_COUNT = 0;

Program *createProgram() {
  Program *program = malloc(sizeof(Program));
  program->id = PROGRAM_COUNT++;
  program->parent = NULL;
  program->capacity = 20;
  program->count = 0;
  program->instructions = calloc(program->capacity, sizeof(Token*));
  program->variableTypes = createHashTable(255);

  return program;
}

Program *createProgramWithParent(Program *parent) {
  Program *program = malloc(sizeof(Program));
  program->id = PROGRAM_COUNT++;
  program->parent = parent;
  program->capacity = 20;
  program->count = 0;
  program->variableTypes = createHashTable(255);

  return program;
}

void deleteProgram(Program *program) {
  for(size_t i = 0;i < program->count;i++) {
    free(program->instructions[i]->data);
    free(program->instructions[i]);
  }
  free(program->instructions);
  deleteHashTable(program->variableTypes);
  free(program);
}

void expandProgramInstructions(Program *program) {
  program->capacity = program->capacity + min(1000, program->capacity);

  Token **instructions = program->instructions;
  program->instructions = calloc(program->capacity, sizeof(Token*));
  for(size_t i = 0;i < program->count;i++) {
    program->instructions[i] = instructions[i];
  }
}

void pushProgramInstruction(Program *program, Token *instruction) {
  if(program->count > program->capacity / 2) {
    expandProgramInstructions(program);
  }
  program->instructions[program->count++] = instruction;
}
Token *popProgramInstruction(Program *program) {
  Token *instruction = program->instructions[--program->count];
  program->instructions[program->count] = NULL;
  return instruction;
}
Token *getProgramInstruction(Program *program, size_t pos, bool remove) {
  Token *instruction = program->instructions[pos];
  if(remove) {
    program->count--;
    if(pos == program->count) {
      program->instructions[pos] = NULL;
    } else {
      for(size_t i = pos;i < program->count;i++) {
        program->instructions[i] = program->instructions[i + 1];
      }
    }
  }
  return instruction;
}

// Changes the createOptions and checks them
void createVariableToken(CreateTokenFromString *createOptions, Token *token) {
  size_t length = createOptions->length;
  bool assignType = createOptions->string[length - 1] == ':';
  if(assignType) {
    length--;
  }

  const char *name = strndup(createOptions->string, length);
  NameData *data = malloc(sizeof(NameData));
  data->variableName = name;
  data->name = name;
  data->assignType = assignType;

  token->type = TOKEN_NAME;
  token->data = data;
}

Token *createToken(Token *createToken) {
  Token *token = malloc(sizeof(Token));
  token->type = createToken->type;
  token->data = createToken->data;
  token->file = createToken->file;
  token->line = createToken->line;
  token->column = createToken->column;

  return token;
}

Token *createTokenFromString(CreateTokenFromString *createOptions) {
  ASSERT(TOKEN_COUNT == 26, "Not all operations are implemented in createTokenFromString!");
  ASSERT(TYPES_COUNT ==  3, "Not all types are implemented in createTokenFromString!");
  Token *token = malloc(sizeof(Token));
  token->file = createOptions->file;
  token->line = createOptions->line;
  token->column = createOptions->column;
  token->data = NULL;

  size_t actualLength = strlen(createOptions->string);
  
  if(strncmp("int", createOptions->string, 3) == 0) {
    createOptions->length -= 3;
    token->type = TOKEN_TYPE;
    token->data = (void*) TYPE_INT;
    return token;
  } else if(strncmp("bool", createOptions->string, 4) == 0) {
    createOptions->length -= 4;
    token->type = TOKEN_TYPE;
    token->data = (void*) TYPE_BOOL;
    return token;
  } else if(strncmp("true", createOptions->string, 4) == 0) {
    createOptions->length -= 4;
    token->type = TOKEN_VALUE;
    ValueData *value = malloc(sizeof(ValueData));
    value->type = TYPE_BOOL;
    value->data = malloc(sizeof(uint8_t));
    *((uint8_t*) value->data) = 1;
    token->data = value;
    return token;
  } else if(strncmp("false", createOptions->string, 5) == 0) {
    createOptions->length -= 5;
    token->type = TOKEN_VALUE;
    ValueData *value = malloc(sizeof(ValueData));
    value->type = TYPE_BOOL;
    value->data = malloc(sizeof(uint8_t));
    *((uint8_t*) value->data) = 0;
    token->data = value;
    return token;
  }

  for(size_t i = 0;true;i++) {
    InstructionType *inst = &INSTRUCTION_TYPES[i];
    if(!inst->name) break;
    if(strncmp(inst->name, createOptions->string, inst->length) == 0) {
      createOptions->length -= inst->length;
      token->type = inst->tokenType;
      return token;
    }
  }

  if(createOptions->last == NULL) {
    // TODO: Add warnings!
    if(isDigit(createOptions->string[0])) {
      createOptions->length = 0;
      free(token);
      return NULL;
    }
    createVariableToken(createOptions, token);
    createOptions->length = 0;
    if(createOptions->error[0] != 0) {
      free(token);
      return NULL;
    }
    return token;
  } else if(createOptions->last != NULL) {
    if(isDigit(createOptions->string[0])) {
      // TODO: Add multiple types support for value token
      token->type = TOKEN_VALUE;
      ValueData *value = malloc(sizeof(ValueData));
      value->type = TYPE_INT;
      value->data = malloc(sizeof(uint32_t));
      *((uint32_t*) value->data) = strnuint32(createOptions->string, createOptions->length);
      token->data = value;
    } else {
      createVariableToken(createOptions, token);
    }
    createOptions->length = 0;
    return token;
  }

  createOptions->length = 0;
  free(token);
  return NULL;
}

size_t isStringTokenFromRight(const char *string, size_t length) {
  ASSERT(TOKEN_COUNT == 26, "Not all operations are implemented in isStringTokenFromRight!");
  ASSERT(TYPES_COUNT ==  3, "Not all types are implemented in isStringTokenFromRight!");
  for(size_t i = 0;true;i++) {
    InstructionType *inst = &INSTRUCTION_TYPES[i];
    if(!inst->name) return 0;
    if(inst->fromRight == false) continue;
    if(rstrncmp(inst->name, inst->length, string, length, inst->length) == 0) {
      return inst->length;
    }
  }
  return 0;
}

Program *createProgramFromFile(const char *filePath, char *error) {
  Program *program = createProgram();
  FILE *in = openFile(filePath, "r");

  char *lineStart = NULL;
  ssize_t lineLength = 0;
  size_t length = 0;
  size_t row = 0;
  CreateTokenFromString createOptions;
  Token *last = NULL;
  createOptions.program = program;
  createOptions.file = filePath;
  createOptions.error = error;
  while((lineLength = getline(&lineStart, &length, in)) != -1) {
    row++;
    length = (size_t) lineLength;
    char *line = lineStart;
    trimRight(line, &length);
    if(length == 0) continue;
    trimLeft(&line, &length);
    for(size_t st = 0; st < length;st++) {
      if(line[st] == '/' && line[st - 1] == '/') {
        length = st - 1;
        break;
      }
    }
    trimLeft(&line, &length);
    size_t start = 0, end = 1;
    const size_t lineLength = length;
    while(start < lineLength) {
      end = 1;
      while(end <= length) {
        if(isspace(line[end - 1])) {
          end--;
          break;
        }
        size_t s = isStringTokenFromRight(line, end);
        if(s == 0 || (end < length - 1 && !(isspace(line[end]) && line[end] != '\0'))) {
          end++;
        } else if(s == end) {
          break;
        } else {
          end -= s;
          break;
        }
      }
      end = min(end, length);

      createOptions.line = row;
      createOptions.column = start + 1;
      createOptions.last = last;
      createOptions.length = end;
      createOptions.string = line;

      last = createTokenFromString(&createOptions);

      if(last == NULL) {
        deleteProgram(program);
        return NULL;
      }

      pushProgramInstruction(program, last);

      if(createOptions.length != 0) {
        line += end - createOptions.length;
        length -= end - createOptions.length;
        size_t tl = trimLeft(&line, &length);
        start += end - createOptions.length + tl;
      } else {
        line += end;
        length -= end;
        size_t tl = trimLeft(&line, &length);
        start += end + tl;
      }
    }
  }

  fclose(in);

  printProgram(program);

  return program;
}

// Instructions

InstructionType INSTRUCTION_TYPES[] = {
  {.name = ":=",         .tokenType = TOKEN_DECLARE,           .length = 2,  .fromRight = true},
  {.name = "=>",         .tokenType = TOKEN_DECLARE_FUNCTION,  .length = 2,  .fromRight = true},
  {.name = ":",          .tokenType = TOKEN_ASSIGN_TYPE,       .length = 1,  .fromRight = true},
  {.name = ";",          .tokenType = TOKEN_SEMICOLON,         .length = 1,  .fromRight = true},
  {.name = "=",          .tokenType = TOKEN_ASSIGN,            .length = 1,  .fromRight = true},
  {.name = "beta_print", .tokenType = TOKEN_PRINT,             .length = 10, .fromRight = false},
  {.name = "+",          .tokenType = TOKEN_ADD,               .length = 1,  .fromRight = true},
  {.name = "-",          .tokenType = TOKEN_SUBTRACT,          .length = 1,  .fromRight = true},
  {.name = ">",          .tokenType = TOKEN_GREATER_THAN,      .length = 1,  .fromRight = true},
  {.name = "<",          .tokenType = TOKEN_LESS_THAN,         .length = 1,  .fromRight = true},
  {.name = "(",          .tokenType = TOKEN_PARENTHESES_OPEN,  .length = 1,  .fromRight = true},
  {.name = ")",          .tokenType = TOKEN_PARENTHESES_CLOSE, .length = 1,  .fromRight = true},
  {.name = "{",          .tokenType = TOKEN_BRACES_OPEN,       .length = 1,  .fromRight = true},
  {.name = "}",          .tokenType = TOKEN_BRACES_CLOSE,      .length = 1,  .fromRight = true},
  {.name = "[",          .tokenType = TOKEN_BRACKETS_OPEN,     .length = 1,  .fromRight = true},
  {.name = "]",          .tokenType = TOKEN_BRACKETS_CLOSE,    .length = 1,  .fromRight = true},
  {.name = "if",         .tokenType = TOKEN_IF,                .length = 2,  .fromRight = false},
  {.name = "else",       .tokenType = TOKEN_ELSE,              .length = 4,  .fromRight = false},
  {.name = "==",         .tokenType = TOKEN_EQUALS,            .length = 2,  .fromRight = true},
  {.name = "!=",         .tokenType = TOKEN_NOT_EQUALS,        .length = 2,  .fromRight = true},
  {.name = "return",     .tokenType = TOKEN_RETURN,            .length = 6,  .fromRight = false},
  {.name = NULL,         .tokenType = -1,                      .length = 0,  .fromRight = false},
};
