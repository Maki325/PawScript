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
        if(program->instructions[i]->type != TOKEN_IF) continue;
        ControlFlowBlock *block = program->instructions[i]->data;
        if(!block) continue;
        block->endInstruction--;
        block->nextInstruction--;
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
  NameValue *value = malloc(sizeof(NameValue));
  value->variableName = name;
  value->name = name;
  value->assignType = assignType;

  token->type = TOKEN_NAME;
  token->data = value;
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
  ASSERT(TOKEN_COUNT == 17, "Not all operations are implemented in createTokenFromString!");
  ASSERT(TYPES_COUNT ==  3, "Not all types are implemented in createTokenFromString!");
  Token *token = malloc(sizeof(Token));
  token->file = createOptions->file;
  token->line = createOptions->line;
  token->column = createOptions->column;
  token->data = NULL;

  size_t actualLength = strlen(createOptions->string);

  if(strncmp("(", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_PARENTHESES_OPEN;
    return token;
  } else if(strncmp(")", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_PARENTHESES_CLOSE;
    return token;
  } else if(strncmp("{", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_BRACES_OPEN;
    return token;
  } else if(strncmp("}", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_BRACES_CLOSE;
    return token;
  } else if(strncmp(";", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_SEMICOLON;
    return token;
  } else if(strncmp("=", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_ASSIGN;
    return token;
  } else if(strncmp("int", createOptions->string, 3) == 0) {
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
  } else if(
    strncmp("print", createOptions->string, 5) == 0 &&
    (actualLength <= 5 || (isspace(createOptions->string[5]) || createOptions->string[5] == '\0'))
  ) {
    createOptions->length -= 5;
    token->type = TOKEN_PRINT;
    return token;
  } else if(strncmp("meow", createOptions->string, 4) == 0) {
    createOptions->length -= 4;
    token->type = TOKEN_PRINT;
    return token;
  } else if(strncmp("<", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_LESS_THAN;
    return token;
  } else if(strncmp(">", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_GREATER_THAN;
    return token;
  } else if(strncmp("+", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_ADD;
    return token;
  } else if(strncmp("-", createOptions->string, 1) == 0) {
    createOptions->length -= 1;
    token->type = TOKEN_SUBTRACT;
    return token;
  } else if(strncmp("if", createOptions->string, 2) == 0) {
    createOptions->length -= 2;
    token->type = TOKEN_IF;
    return token;
  } else if(createOptions->last == NULL) {
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

void checkInstruction(Program *program, Token *instruction) {
  if(program->count == 0) return;
  Token *last = program->instructions[program->count - 1];
  if(last->type == TOKEN_NAME) {
    NameValue *value = instruction->data;
    NameValue *lastValue = last->data;
    if(lastValue->assignType && instruction->type != TOKEN_TYPE) {
      fprintf(stderr, "ERROR: ");
      printTokenLocation(instruction, stderr);
      fprintf(stderr, ": Token `%s` is not a type!\n", value->variableName);
      exit(1);
      return;
    }
  }
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

size_t isStringTokenFromRight(const char *string, size_t length) {
  ASSERT(TOKEN_COUNT == 17, "Not all operations are implemented in isStringTokenFromRight!");
  ASSERT(TYPES_COUNT ==  3, "Not all types are implemented in isStringTokenFromRight!");
  if(rstrncmp("if", 2, string, length, 2) == 0) {
    return 2;
  } else if(rstrncmp("(", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp(")", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp("{", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp("}", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp(";", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp("=", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp("int", 3, string, length, 3) == 0) {
    return 3;
  } else if(rstrncmp("bool", 4, string, length, 4) == 0) {
    return 4;
  } else if(rstrncmp("true", 4, string, length, 4) == 0) {
    return 4;
  } else if(rstrncmp("false", 5, string, length, 5) == 0) {
    return 5;
  } /*else if(rstrncmp("print", 5, string, length, 5) == 0) {
    return 5;
  } else if(rstrncmp("meow", 4, string, length, 4) == 0) {
    return 4;
  } */else if(rstrncmp("<", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp(">", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp("+", 1, string, length, 1) == 0) {
    return 1;
  } else if(rstrncmp("-", 1, string, length, 1) == 0) {
    return 1;
  }
  return 0;
}

int typesetProgram(Program *program) {
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i], *next;
    if(token->type == TOKEN_SCOPE) {
      int code;
      if((code = typesetProgram((Program*)token->data)))
        return code;
    } else if(token->type == TOKEN_SCOPE) {
      ControlFlowBlock *block = token->data;
      int code;
      Program prog = {.instructions=&block->condition, .count = 1};
      if((code = typesetProgram(&prog))) {
        return code;
      }
      if((code = typesetProgram((Program*)block->program)))
        return code;
    }
    if(token->type != TOKEN_NAME) {
      continue;
    }
    NameValue *value = token->data;

    if(value->assignType && i == program->count - 1) {
      // Error: Variable without a type
      return -1;
    }
    if(i == program->count - 1 && value->type == TYPE_NONE) {
      // Error: Variable without a type
      fprintf(stderr, "ERROR: ");
      printTokenLocation(token, stderr);
      fprintf(stderr, ": Variable `%s` without a type!\n", value->variableName);
      exit(-1);
      return -1;
    }
    next = program->instructions[i + 1];
    switch (next->type) {
      case TOKEN_TYPE: {
        if(!value->assignType) {
          // Error: Wrong syntax (No `:`)! Expected `var: type` got `var type`
          fprintf(stderr, "ERROR: ");
          printTokenLocation(token, stderr);
          fprintf(stderr, ": Wrong syntax (No `:`)! Expected `%s: type` got `%s type`!\n", value->variableName, value->variableName);
          exit(-4);
          return -4;
        } else if(*value->type == TYPE_NONE) {
          *value->type = (Type) next->data;
        } else if(*value->type != (Type) next->data) {
          // Error: Wrong type reasignment
          fprintf(stderr, "ERROR: ");
          printTokenLocation(token, stderr);
          fprintf(stderr, ": Wrong type reasignment for variable `%s`!\n", value->variableName);
          exit(-3);
          return -3;
        }
        free(getProgramInstruction(program, i + 1, true));
        break;
      }
      case TOKEN_ASSIGN: {
        next = program->instructions[i + 2];
        switch (next->type) {
          case TOKEN_VALUE: {
            ValueData *data = next->data;
            if(*value->type == TYPE_NONE) {
              // TODO: Add multiple types support for value token
              *value->type = data->type;
              break;
            } else if(*value->type != data->type) {
              // TODO: Add multiple types support for value token
              // Error: Wrong type reasignment
              fprintf(stderr, "ERROR: ");
              printTokenLocation(token, stderr);
              fprintf(stderr,
                      ": Wrong type reasignment for variable `%s`! Expected `%s` got `%s`!\n",
                      value->variableName,
                      getTypeName(*value->type),
                      getTypeName(data->type));
              exit(-3);
              return -3;
            }
            break;
          }
          case TOKEN_NAME: {
            NameValue *nextValue = next->data;
            if(*value->type == TYPE_NONE) {
              *value->type = *nextValue->type;
              break;
            } else if(*value->type != *nextValue->type) {
              // Error: Wrong type reasignment
              fprintf(stderr, "ERROR: ");
              printTokenLocation(token, stderr);
              fprintf(stderr, ": Wrong type reasignment for variable `%s`!\n", value->variableName);
              exit(-3);
              return -3;
            }
            break;
          }
          
          default:
            break;
        }
        break;
      }
      default: {
        if(value->type == TYPE_NONE) {
          fprintf(stderr, "ERROR: ");
          printTokenLocation(token, stderr);
          fprintf(stderr, ": Undeclared variable `%s`!\n", value->variableName);
          exit(-2);
          // Error: Undeclared variable
          return -2;
        } else {
          break;
        }
      }
    }
  }

  return 0;
}

int crossrefrenceBlocks(Program *program) {
  ASSERT(TOKEN_COUNT == 17, "Not all operations are implemented in crossrefrenceProgram!");
  size_t refrences[program->count], count = 0;
  Token token;
  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];
    switch (instruction->type) {
      case TOKEN_PARENTHESES_OPEN: {
        refrences[count++] = i + 1;
        break;
      }
      case TOKEN_PARENTHESES_CLOSE: {
        size_t start = refrences[--count],
          length = i - start, pos = 0;
        if(getProgramInstruction(program, start - 1, false)->type != TOKEN_PARENTHESES_OPEN) {
          fprintf(stderr, "ERROR: Blocks are not balanced at: %s:%zu%zu",
                    instruction->file, instruction->line, instruction->column);
          exit(1);
          return -1;
        }
        Token **tokens = calloc(length, sizeof(Token*));
        for(size_t j = 0;j < length;j++) {
          tokens[pos++] = getProgramInstruction(program, start, true);
        }
        // Get `(` token for location
        Token *startToken = getProgramInstruction(program, start - 1, true);
        token.file = startToken->file;
        token.line = startToken->line;
        token.column = startToken->column;
        token.type = TOKEN_PRIORITY;
        TokenPriorityValue *value = malloc(sizeof(TokenPriorityValue));
        value->instructions = tokens;
        value->count = length;
        token.data = value;
        free(startToken);

        // Replace `)` token with actual priority token
        free(getProgramInstruction(program, start - 1, false));
        program->instructions[start - 1] = createToken(&token);
        i = start - 1;

        if(count != 0 && program->instructions[refrences[count - 1]]->type == TOKEN_IF) {
          Token *ifToken = program->instructions[refrences[count - 1]];
          ControlFlowBlock *block = malloc(sizeof(ControlFlowBlock));
          ifToken->data = block;

          block->condition = getProgramInstruction(program, start - 1, true);
          block->nextInstruction = 0;
          block->endInstruction = 0;
          block->program = NULL;
          i = start - 2;
        }

        break;
      }

      case TOKEN_BRACES_OPEN: {
        refrences[count++] = i + 1;
        Program *inside = createProgramWithParent(program);
        instruction->data = inside;
        break;
      }
      case TOKEN_BRACES_CLOSE: {
        size_t start = refrences[--count],
          length = i - start;
        Token *openInstruction = getProgramInstruction(program, start - 1, false);
        if(openInstruction->type != TOKEN_BRACES_OPEN) {
          fprintf(stderr, "ERROR: Blocks are not balanced at: %s:%zu%zu",
                    instruction->file, instruction->line, instruction->column);
          exit(1);
          return -1;
        }
        Program *inside = openInstruction->data;
        inside->count = length;
        inside->capacity = length;
        Token **tokens = inside->instructions = calloc(length, sizeof(Token*));
        for(size_t j = 0;j < length;j++) {
          tokens[j] = getProgramInstruction(program, start, true);
          if(tokens[j]->type != TOKEN_IF) continue;
          ControlFlowBlock *block = tokens[j]->data;
          if(!block) continue;
          block->endInstruction -= start;
          block->nextInstruction -= start;
        }
        // Get `{` token for location
        Token *startToken = getProgramInstruction(program, start - 1, true);
        token.file = startToken->file;
        token.line = startToken->line;
        token.column = startToken->column;
        token.type = TOKEN_SCOPE;
        token.data = inside;
        free(startToken);

        //Getting the parent
        if(count == 0) {
          inside->parent = program;
        } else {
          for(int q = count - 1;q >= 0;q--) {
            Token *tok = program->instructions[refrences[q] - 1];
            if(tok->type == TOKEN_BRACES_OPEN) {
              inside->parent = tok->data;
              break;
            }
          }
          if(inside->parent == NULL) inside->parent = program;
        }

        // Replace `}` token with actual priority token
        free(getProgramInstruction(program, start - 1, false));
        program->instructions[start - 1] = createToken(&token);
        i = start - 1;

        if(count != 0 && program->instructions[refrences[count-1]]->type == TOKEN_IF) {
          Token *ifToken = program->instructions[refrences[--count]];
          ControlFlowBlock *block = ifToken->data;
          if(!block) {
            fprintf(stderr, "ERROR: No condition for if statement at %s:%zu:%zu!\n",
                            ifToken->file, ifToken->line, ifToken->column);
            exit(1);
            return -1;
          }
          if(block->program) break;
          Token *scope = getProgramInstruction(program, start - 1, true);
          block->program = scope->data;
          for(size_t j = 0;j < block->program->count;j++) {
            if(block->program->instructions[j]->type != TOKEN_IF) continue;
            ControlFlowBlock *innerBlock = block->program->instructions[j]->data;
            if(!innerBlock) continue;
            innerBlock->endInstruction += 3;
            innerBlock->nextInstruction += 3;
          }
          free(scope);
          i = start - 2;

          block->endInstruction = i + 1;
          block->nextInstruction = i + 1;
        }

        break;
      }
      
      case TOKEN_IF: {
        refrences[count++] = i;
        break;
      }
      default:
        break;
    }
  }

  if(count != 0) {
    PSLOG("Count: %zu\n", count);
    for(size_t i = 0;i < count;i++) {
      PSLOG("Ref[%zu]: %zu\n", i, refrences[i]);
    }
    // Not all blocks are closed!
    return -1;
  }

  return 0;
}

int crossrefrenceVariables(Program *program, HashTable *parentNameMap) {
  ASSERT(TOKEN_COUNT == 17, "Not all operations are implemented in crossrefrenceVariables!");
  HashTable *nameMap = parentNameMap == NULL ? createHashTable(255) : createHashTableFrom(parentNameMap);
  for(size_t i = 0; i < program->count;i++) {
    Token *instruction = program->instructions[i];
    if(instruction->type == TOKEN_SCOPE) {
      int code = crossrefrenceVariables(instruction->data, nameMap);
      if(code != 0) return code;
      continue;
    } else if(instruction->type == TOKEN_PRIORITY) {
      TokenPriorityValue *value = instruction->data;
      Program prog = {.instructions = value->instructions, .count = value->count};
      int code = crossrefrenceVariables(&prog, nameMap);
      if(code != 0) return code;
      continue;
    } else if(instruction->type == TOKEN_IF) {
      ControlFlowBlock *block = instruction->data;

      int code = crossrefrenceVariables(block->program, nameMap);
      if(code != 0) return code;
      Program prog = {.instructions = &block->condition, .count = 1};
      code = crossrefrenceVariables(&prog, nameMap);
      if(code != 0) return code;

      continue;
    }
    if(instruction->type != TOKEN_NAME) continue;
    NameValue *value = instruction->data;
    const char *name = value->name;
    if(existsElementInHashTable(nameMap, name)) {
      NameMapValue *element = getElementFromHashTable(nameMap, name);
      if(!value->assignType || element->program == program) {
        value->name = element->name;
        value->type = element->type;
        continue;
      }
    }
    NameMapValue *element = malloc(sizeof(NameMapValue));
    element->program = program;

    Type *type = malloc(sizeof(Type));
    *type = TYPE_NONE;
    element->type = type;

    const size_t newNameLength = 4 +
          (program->id == 0 ? 1 : ((int) log10(program->id) + 1)) +
          1 + (i == 0 ? 1 : ((int) log10(i) + 1)) + 1;
    char *newName = calloc(newNameLength, sizeof(char));
    snprintf(newName, newNameLength, "var_%zu_%zu", program->id, i);
    element->name = newName;

    setElementInHashTable(nameMap, name, element);

    value->name = newName;
    value->type = type;
  }

  deleteHashTable(nameMap);

  return 0;
}

bool canBeUsedInArithmeticOperations(TokenType type) {
  return type == TOKEN_NAME || type == TOKEN_VALUE || type == TOKEN_ADD
          || type == TOKEN_SUBTRACT || type == TOKEN_PRIORITY;
}
bool canBeUsedInComparisonOperations(TokenType type) {
  return type == TOKEN_NAME || type == TOKEN_VALUE || type == TOKEN_ADD
          || type == TOKEN_SUBTRACT || type == TOKEN_PRIORITY
          || type == TOKEN_GREATER_THAN || type == TOKEN_LESS_THAN;
}

int crossrefrencePriority(Token **holder, size_t *iPtr) {
  Token *instruction = *holder;

  TokenPriorityValue *value = instruction->data;
  
  int errorCode = 8888;
  if(value->count > 1) {
    Program prog = {.instructions = value->instructions, .count = value->count};
    errorCode = crossrefrenceOperations(&prog);
    value->count = prog.count;
  }
  
  if(value->count == 1) {
    (*holder) = value->instructions[0];
    if(iPtr != NULL && errorCode == 8888) {
      (*iPtr) = (*iPtr) - 1;
    }
    free(value->instructions);
    free(value);
    free(instruction);
  }

  return 0;
}

int crossrefrenceOperations(Program *program) {
  ASSERT(TOKEN_COUNT == 17, "Not all operations are implemented in crossrefrenceOperations!");
  ASSERT(TYPES_COUNT ==  3, "Not all types are implemented in crossrefrenceOperations!");
  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];
    switch (instruction->type) {
      case TOKEN_IF: {
        ControlFlowBlock *block = instruction->data;
        Program prog = {.instructions = &block->condition, .count = 1};
        crossrefrenceOperations(&prog);
        crossrefrenceOperations((Program*) block->program);
        break;
      }
      case TOKEN_PRIORITY: {
        crossrefrencePriority(&program->instructions[i], &i);
        break;
      }
      case TOKEN_SCOPE: {
        crossrefrenceOperations((Program*) instruction->data);
        break;
      }
      case TOKEN_ADD:
      case TOKEN_SUBTRACT: {
        if(instruction->data) {
          BinaryOperationValue *value = instruction->data;
          if(value->operandOne && value->operandTwo) break;
          else if(value->operandOne || value->operandTwo) {
            fprintf(stderr, "Operation{pos: %zu, type: \"%s\"} doesn't have both operands!\n", i, getTokenTypeName(instruction->type));
            exit(1);
          }
        }
        if(i == 0 || i == program->count - 1) {
          // Not enought operands for token!
          return -1;
        }

        Token *left  = getProgramInstruction(program, i - 1, false),
              *right = getProgramInstruction(program, i + 1, false);
        if(!canBeUsedInArithmeticOperations(left->type)) {
          printf("Op 1 cant be used! I: %zu, type[i-1]: %d\n", i, getProgramInstruction(program, i - 1, false)->type);
          return i;
        }
        if(!canBeUsedInArithmeticOperations(right->type)) {
          printf("Op 2 cant be used! I: %zu\n", i);
          return i;
        }
        if(left->type == TOKEN_PRIORITY) {
          crossrefrencePriority(&program->instructions[i - 1], NULL);
        }
        if(right->type == TOKEN_PRIORITY) {
          crossrefrencePriority(&program->instructions[i + 1], NULL);
        }
        BinaryOperationValue *value = instruction->data = malloc(sizeof(BinaryOperationValue));

        value->operandTwo = getProgramInstruction(program, i + 1, true);
        value->operandOne = getProgramInstruction(program, i - 1, true);
        i -= 1;

        break;
      }
      default:
        break;
    }
  }
  
  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];
    switch (instruction->type) {
      case TOKEN_GREATER_THAN:
      case TOKEN_LESS_THAN: {
        if(i == 0 || i == program->count - 1) {
          // Not enought operands for token!
          return -1;
        }

        Token *left  = getProgramInstruction(program, i - 1, false),
              *right = getProgramInstruction(program, i + 1, false);
        if(!canBeUsedInComparisonOperations(left->type)) {
          printf("Op 1 cant be used! I: %zu, type[i-1]: %d\n", i, getProgramInstruction(program, i - 1, false)->type);
          return i;
        }
        if(!canBeUsedInComparisonOperations(right->type)) {
          printf("Op 2 cant be used! I: %zu\n", i);
          return i;
        }
        
        // NOTE: Since the crossrefrences are done above
        // No need to redo them?
        BinaryOperationValue *value = instruction->data = malloc(sizeof(BinaryOperationValue));
        value->operandTwo = getProgramInstruction(program, i + 1, true);
        value->operandOne = getProgramInstruction(program, i - 1, true);
        i -= 1;

        break;
      }
      default:
        break;
    }
  }

  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];
    switch (instruction->type) {
      case TOKEN_PRINT: {
        if(i == program->count - 1) {
          // Not enought operands for token!
          return -1;
        }

        Token *right = getProgramInstruction(program, i + 1, true);
        NameValue *value = right->data;
        free(right);
        instruction->data = (void*) value->name;
        free(value);

        break;
      }
      default:
        break;
    }
  }

  return 0;
}

Program *createProgramFromFile(const char *filePath, char *error) {
  Program *program = createProgram();
  FILE *in = fopen(filePath, "r");

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
      checkInstruction(program, last);

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

  crossrefrenceBlocks(program); // Moved it before typeset because there can exist multiple scopes
  crossrefrenceVariables(program, NULL);
  int typesetError = typesetProgram(program);
  if(typesetError != 0) {
    fprintf(stderr, "ERROR: Typeset error: %d\n", typesetError);
    exit(typesetError);
  }
  crossrefrenceOperations(program);

  Token *token = checkProgram(program);
  if(token) {
    snprintf(
      error, 512,
      "%s:%zu:%zu: Variable `%s` doesn't have a type!",
      token->file, token->line, token->column,
      ((NameValue*)token->data)->variableName
    );
    return NULL;
  }

  return program;
}
