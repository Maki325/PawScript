#include "tokenizer.h"
#include "utils/utils.h"

Program *createProgram() {
  Program *program = malloc(sizeof(Program));
  program->capacity = 20;
  program->count = 0;
  program->instructions = calloc(program->capacity, sizeof(Token*));
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

int trimLeft(char **text, size_t length) {
  size_t i = 0;
  while(isspace((*text)[0])) {
    (*text)++;
  }
  return i;
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
  ASSERT(TOKEN_COUNT == 8, "Not all operations are implemented in createTokenFromString!");
  Token token = {};
  token.file = createOptions->file;
  token.line = createOptions->line;
  token.column = createOptions->column;
  token.data = NULL;

  bool isPlus = false;

  if(strncmp("int", createOptions->string, 3) == 0) {
    token.type = TOKEN_TYPE;
    token.data = TYPE_INT;
    return createToken(&token);
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
  } else if(
    (isPlus = (strncmp("+", createOptions->string, 1) == 0)) ||
    strncmp("-", createOptions->string, 1) == 0
  ) {
    if(createOptions->last->type == NULL) {
      snprintf(
        createOptions->error, 512,
        "%s:%zu:%zu: + Operation needs to have a token before it!",
        createOptions->file, createOptions->line, createOptions->column
      );
      return;
    }
    Token *instructionOne = popProgramInstruction(createOptions->program);
    token.type = isPlus ? TOKEN_ADD : TOKEN_SUBTRACT;
    token.data = (BinaryOperationValue*) malloc(sizeof(BinaryOperationValue));
    ((BinaryOperationValue*) token.data)->operandOne = instructionOne;
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
          token.type = TOKEN_NAME;
        }
        Token *right = createToken(&token);
        BinaryOperationValue *value = createOptions->last->data;
        value->operandTwo = right;

        return NULL;
      }
      case TOKEN_TYPE:
        token.type = TOKEN_NAME;
        token.data = data;
        return createToken(&token);
      case TOKEN_PRINT:
        createOptions->last->data = data;
        return NULL;
      case TOKEN_ASSIGN:
        token.data = data;
        if(isDataDigit) {
          token.type = TOKEN_VALUE;
        } else {
          token.type = TOKEN_NAME;
        }
        return createToken(&token);
      default: {
        free(data);
      }
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
    char *line = lineStart;
    for(size_t st = 1; st < length;st++) {
      if(line[st] == '/' && line[st - 1] == '/') {
        length = st - 1;
        break;
      }
    }
    size_t start = trimLeft(&line, length), end = 1;
    while(start < length) {
      end = 1;
      while(
        end + start < length &&
        !isspace(line[end]) && line[end] != ';'
      ) {
        end++;
      }

      createOptions.line = row;
      createOptions.column = start;
      createOptions.last = last;
      createOptions.length = end;
      createOptions.string = line;

      last = createTokenFromString(&createOptions);
      if(error[0] != 0) return 0;
      if(last == NULL) {
        last = program->instructions[program->count - 1];
      } else {
        pushProgramInstruction(program, last);
      }
      line += end;
      size_t tl = trimLeft(&line, length);
      start += end + tl + 1;
    }
    row++;
  }

  fclose(in);
  return program;
}

