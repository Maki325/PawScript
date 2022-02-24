#include "tokenizer.h"
#include "utils/utils.h"

Program *createProgram() {
  Program *program = malloc(sizeof(Program));
  program->capacity = 20;
  program->count = 0;
  program->instructions = calloc(program->capacity, sizeof(Token*));
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
Token *getProgramInstruction(Program *program, size_t i) {
  return program->instructions[i];
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

// Changes the createOptions and checks them
void createVariableToken(CreateTokenFromString *createOptions, Token *token) {
  HashTable *variableTypes = createOptions->program->variableTypes;

  size_t length = createOptions->length;
  bool assignType = createOptions->string[length - 1] == ':';
  if(assignType) {
    length--;
  }

  const char *name = strndup(createOptions->string, length);
  NameValue *value = malloc(sizeof(NameValue));
  value->name = name;
  value->assignType = assignType;
  if(existsElementInHashTable(variableTypes, name)) {
    if(assignType) {
      snprintf(
        createOptions->error, 512,
        "%s:%zu:%zu: Trying to reassign a variable `%s` to another type!",
        createOptions->file, createOptions->line, createOptions->column,
        name
      );
      return;
    }
    value->type = getElementFromHashTable(variableTypes, name);
  } else {
    Type *type = malloc(sizeof(Type));
    *type = TYPE_NONE;
    setElementInHashTable(variableTypes, name, type);
    value->type = type;
  }

  token->type = TOKEN_NAME;
  token->data = value;
}

Token *createTokenFromString(CreateTokenFromString *createOptions) {
  ASSERT(TOKEN_COUNT == 9, "Not all operations are implemented in createTokenFromString!");
  HashTable *variableTypes = createOptions->program->variableTypes;
  Token token = {};
  token.file = createOptions->file;
  token.line = createOptions->line;
  token.column = createOptions->column;
  token.data = NULL;

  bool isPlus = false, isGreater = false;

  if(strncmp("int", createOptions->string, 3) == 0) {
    if(createOptions->last == NULL) {
      snprintf(
        createOptions->error, 512,
        "%s:%zu:%zu: Type needs to have a name token before it!",
        createOptions->file, createOptions->line, createOptions->column
      );
      return;
    } else if(createOptions->last->type == TOKEN_NAME) {
      NameValue *value = createOptions->last->data;
      if(value->assignType == false) {
        snprintf(
          createOptions->error, 512,
          "%s:%zu:%zu: Types should only go after!",
          createOptions->file, createOptions->line, createOptions->column,
          value->name
        );
        return NULL;
      }
      value->type = getElementFromHashTable(variableTypes, value->name);
      if(*value->type != TYPE_NONE) {
        snprintf(
          createOptions->error, 512,
          "%s:%zu:%zu: Trying to reassign a variable `%s` to another type!",
          createOptions->file, createOptions->line, createOptions->column,
          value->name
        );
        return NULL;
      } else {
        *(value->type) = TYPE_INT;
      }
    }

    return NULL;
  } else if(strncmp(";", createOptions->string, 1) == 0) {
    token.type = TOKEN_SEMICOLON;
    return createToken(&token);
  } else if(strncmp("=", createOptions->string, 1) == 0) {
    token.data = strdup(createOptions->last->data);
    token.type = TOKEN_ASSIGN;
    return createToken(&token);
  } else if(
    strncmp("print", createOptions->string, 5) == 0 ||
    strncmp("meow", createOptions->string, 4) == 0
  ) {
    token.type = TOKEN_PRINT;
    return createToken(&token);
  } 
  else if(
    (isGreater = (strncmp(">", createOptions->string, 1) == 0)) ||
    strncmp("<", createOptions->string, 1) == 0
  ) {
    if(createOptions->last == NULL) {
      snprintf(
        createOptions->error, 512,
        "%s:%zu:%zu: > Operation needs to have a token before it!",
        createOptions->file, createOptions->line, createOptions->column
      );
      return;
    }
    token.type = isGreater ? TOKEN_GREATER_THAN : TOKEN_LESS_THAN;
    BinaryOperationValue *data = malloc(sizeof(BinaryOperationValue));
    token.data = data;
    if(
      createOptions->last->type == TOKEN_VALUE ||
      createOptions->last->type == TOKEN_NAME ||
      createOptions->last->type == TOKEN_ADD ||
      createOptions->last->type == TOKEN_SUBTRACT
    ) {
      data->operandOne = popProgramInstruction(createOptions->program);
      return createToken(&token);
    } else if(createOptions->last->type == TOKEN_PRINT) {
      Token *printInstruction = createOptions->last;
      data->operandOne = printInstruction->data;
      printInstruction->data = createToken(&token);
      return NULL;
    }
  }
  else if(
    (isPlus = (strncmp("+", createOptions->string, 1) == 0)) ||
    strncmp("-", createOptions->string, 1) == 0
  ) {
    if(createOptions->last == NULL) {
      snprintf(
        createOptions->error, 512,
        "%s:%zu:%zu: + Operation needs to have a token before it!",
        createOptions->file, createOptions->line, createOptions->column
      );
      return;
    }
    Token *instructionOne;

    token.type = isPlus ? TOKEN_ADD : TOKEN_SUBTRACT;
    BinaryOperationValue *data = malloc(sizeof(BinaryOperationValue));
    token.data = data;

    if(createOptions->last->type == TOKEN_GREATER_THAN || createOptions->last->type == TOKEN_LESS_THAN) {
      instructionOne = createOptions->last;
      BinaryOperationValue *lg = instructionOne->data;

      if(lg->operandTwo->type == TOKEN_ADD || lg->operandTwo->type == TOKEN_SUBTRACT) {
        BinaryOperationValue *pm = lg->operandTwo->data;
        if(pm->operandTwo == NULL) {
          snprintf(
            createOptions->error, 512,
            "%s:%zu:%zu: Not enough arguments!",
            createOptions->file, createOptions->line, createOptions->column
          );
          return NULL;
        } else {
          data->operandOne = lg->operandTwo;
          lg->operandTwo = createToken(&token);
          return NULL;
        }
      } else {
        data->operandOne = lg->operandTwo;
        lg->operandTwo = createToken(&token);
      }
      return NULL;
    } else {
      instructionOne = popProgramInstruction(createOptions->program);

      data->operandOne = instructionOne;
      return createToken(&token);
    }
  } else if(createOptions->last == NULL) {
    // TODO: Add warnings!
    if(isDigit(createOptions->string[0])) return NULL;
    createVariableToken(createOptions, &token);
    if(createOptions->error[0] != 0) return NULL;

    return createToken(&token);
  } else if(createOptions->last != NULL) {
    void *data = NULL;
    bool isDataDigit = isDigit(createOptions->string[0]);
    if(isDataDigit) {
      int *value = malloc(sizeof(int));
      (*value) = strnint(createOptions->string, createOptions->length);
      data = value;
    } else {
      data = strndup(createOptions->string, createOptions->length);
    }
    switch (createOptions->last->type) {
      case TOKEN_ADD:
      case TOKEN_SUBTRACT: {
        token.data = data;
        if(isDataDigit) {
          token.type = TOKEN_VALUE;
        } else {
          createVariableToken(createOptions, &token);
          if(createOptions->error[0] != 0) return NULL;
          token.type = TOKEN_NAME;
        }
        Token *right = createToken(&token);
        BinaryOperationValue *value = createOptions->last->data;
        value->operandTwo = right;

        return NULL;
      }
      case TOKEN_GREATER_THAN:
      case TOKEN_LESS_THAN: {
        token.data = data;
        if(isDataDigit) {
          token.type = TOKEN_VALUE;
        } else {
          createVariableToken(createOptions, &token);
          if(createOptions->error[0] != 0) return NULL;
          token.type = TOKEN_NAME;
        }
        Token *right = createToken(&token);
        BinaryOperationValue *value = createOptions->last->data;
        if(value->operandTwo != NULL) {
          if(
            value->operandTwo->type == TOKEN_ADD ||
            value->operandTwo->type == TOKEN_SUBTRACT
          ) {
            BinaryOperationValue *data = value->operandTwo->data;
            data->operandTwo = right;
          } else {
            snprintf(
              createOptions->error, 512,
              "%s:%zu:%zu: Too much arguments for > operation!",
              createOptions->file, createOptions->line, createOptions->column
            );
            return;
          }
        } else {
          value->operandTwo = right;
        }

        return NULL;
      }
      case TOKEN_SEMICOLON: {
        createVariableToken(createOptions, &token);
        if(createOptions->error[0] != 0) return NULL;
        return createToken(&token);
      }
      case TOKEN_PRINT: {
        createOptions->last->data = data;
        return NULL;
      }
      case TOKEN_ASSIGN:{
        token.data = data;

        Token *name = getProgramInstruction(createOptions->program, createOptions->program->count - 2);
        NameValue* nameValue = name->data;

        ASSERT(TYPES_COUNT == 2, "Check for other types!");
        if(isDataDigit) {
          token.type = TOKEN_VALUE;
          if(nameValue->type && *(nameValue->type) != TYPE_NONE) {
            // TODO: Other types!!!
            if(*(nameValue->type) != TYPE_INT) {
              snprintf(
                createOptions->error, 512,
                "%s:%zu:%zu: Trying to reassign a variable `%s` to another type!",
                createOptions->file, createOptions->line, createOptions->column,
                nameValue->name
              );
              return;
            }
          } else {
            *(nameValue->type) = TYPE_INT;
          }
        } else {
          createVariableToken(createOptions, &token);
          if(createOptions->error[0] != 0) return NULL;
          NameValue *other = token.data;
          if(nameValue->type && *(nameValue->type) != TYPE_NONE) {
            // TODO: Other types!!!
            if(*(nameValue->type) != *(other->type)) {
              snprintf(
                createOptions->error, 512,
                "%s:%zu:%zu: Trying to reassign a variable `%s` to another type!",
                createOptions->file, createOptions->line, createOptions->column,
                nameValue->name
              );
              return 0;
            }
          } else {
            *(nameValue->type) = *(other->type);
          }
        }
        return createToken(&token);
      }
      default: {
        free(data);
      }
    }
  }
  return NULL;
}

Token *checkProgram(Program *program) {
  for(size_t i = 0; i < program->count;i++) {
    Token *instruction = program->instructions[i];
    if(instruction->type == TOKEN_ADD || instruction->type == TOKEN_SUBTRACT) {

    }
    if(instruction->type != TOKEN_NAME) continue;
    NameValue *value = instruction->data;
    if(*(value->type) == TYPE_NONE) {
      return instruction;
    }
  }
  return NULL;
}

Program *createProgramFromFile(const char *filePath, char *error) {
  Program *program = createProgram();
  FILE *in = fopen(filePath, "r");

  char *lineStart = NULL;
  size_t length = 0;
  size_t row = 0;
  CreateTokenFromString createOptions;
  Token *last = NULL;
  createOptions.program = program;
  createOptions.file = filePath;
  createOptions.error = error;
  while((length = getline(&lineStart, &length, in)) != -1) {
    if(length == 0) continue;
    char *line = lineStart;
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
      while(
        end < length &&
        !isspace(line[end]) && line[end] != ';'
      ) {
        end++;
      }

      createOptions.line = row + 1;
      createOptions.column = start + 1;
      createOptions.last = last;
      createOptions.length = end;
      createOptions.string = line;

      last = createTokenFromString(&createOptions);
      if(error[0] != 0) {
        deleteProgram(program);
        return 0;
      }
      if(last == NULL) {
        last = program->instructions[program->count - 1];
      } else {
        pushProgramInstruction(program, last);
      }
      line += end;
      size_t tl = trimLeft(&line, &length);
      start += end + tl;
    }
    row++;
  }

  fclose(in);

  Token *token = checkProgram(program);
  if(token) {
    snprintf(
      error, 512,
      "%s:%zu:%zu: Variable `%s` doesn't have a type!",
      token->file, token->line, token->column,
      ((NameValue*)token->data)->name
    );
    return NULL;
  }

  return program;
}
