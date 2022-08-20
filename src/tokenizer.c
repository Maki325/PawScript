#include "tokenizer.h"
#include "pawscript_error.h"
#include "utils/utils.h"
#include <stdarg.h>

size_t PROGRAM_COUNT = 0;

Program *createProgram() {
  Program *program = malloc(sizeof(Program));
  program->id = PROGRAM_COUNT++;
  program->parent = NULL;
  program->capacity = 20;
  program->count = 0;
  program->instructions = calloc(program->capacity, sizeof(Token*));
  program->functions = createHashTable(255);

  return program;
}

Program *createProgramWithParent(Program *parent) {
  Program *program = malloc(sizeof(Program));
  program->id = PROGRAM_COUNT++;
  program->parent = parent;
  program->capacity = 20;
  program->count = 0;
  program->functions = createHashTable(255);

  return program;
}

void deleteProgram(Program *program) {
  for(size_t i = 0;i < program->count;i++) {
    free(program->instructions[i]->data);
    free(program->instructions[i]);
  }
  free(program->instructions);
  deleteHashTable(program->functions);
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

  const char *name = strndup(createOptions->string, length);
  NameData *data = malloc(sizeof(NameData));
  data->variableName = name;
  data->name = name;
  data->mutable = false;

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
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in createTokenFromString!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in createTokenFromString!");
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
  } else if(strncmp("void", createOptions->string, 4) == 0) {
    createOptions->length -= 4;
    token->type = TOKEN_TYPE;
    token->data = (void*) TYPE_VOID;
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
      value->data = malloc(sizeof(uint64_t));
      *((uint64_t*) value->data) = strnuint64(createOptions->string, createOptions->length);
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
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in isStringTokenFromRight!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in isStringTokenFromRight!");
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

int crossreferenceBlocks(Program *program) {
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in crossrefrenceProgram!");
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
          fprintf(stderr, "ERROR: Parentheses blocks are not balanced at: %s:%zu:%zu\n",
                    instruction->file, instruction->line, instruction->column);
          exit(ERROR_PARENTHESES_NOT_BALANCED);
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
        TokenPriorityData *value = malloc(sizeof(TokenPriorityData));
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
          fprintf(stderr, "ERROR: Braces blocks are not balanced at: %s:%zu:%zu\n",
                    instruction->file, instruction->line, instruction->column);
          exit(ERROR_BRACES_NOT_BALANCED);
          return -1;
        }
        Program *inside = openInstruction->data;
        inside->count = length;
        inside->capacity = length;
        Token **tokens = inside->instructions = calloc(length, sizeof(Token*));
        
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

        for(size_t j = 0;j < length;j++) {
          tokens[j] = getProgramInstruction(program, start, true);
        }
        // Get `{` token for location
        Token *startToken = getProgramInstruction(program, start - 1, true);
        token.file = startToken->file;
        token.line = startToken->line;
        token.column = startToken->column;
        token.type = TOKEN_SCOPE;
        token.data = inside;
        free(startToken);

        // Replace `}` token with actual priority token
        free(getProgramInstruction(program, start - 1, false));
        program->instructions[start - 1] = createToken(&token);
        i = start - 1;

        if(count != 0) {
          switch(program->instructions[refrences[count-1]]->type) {
            case TOKEN_IF: {
              size_t newStart = refrences[--count];
              Token *ifToken = program->instructions[newStart];
              ControlFlowBlock *block = ifToken->data;
              if(!block) {
                fprintf(stderr, "ERROR: No condition for if statement at %s:%zu:%zu!\n",
                                ifToken->file, ifToken->line, ifToken->column);
                exit(ERROR_IF_NO_CONDITION);
                return -1;
              }
              if(block->program) break;
              Token *scope = getProgramInstruction(program, start - 1, true);
              block->program = scope->data;
              free(scope);
              i = newStart;

              block->endInstruction = 1;
              block->nextInstruction = 1;

              if(count != 0 && program->instructions[refrences[count-1]]->type == TOKEN_ELSE) {
                count--;
              }
              break;
            }
            case TOKEN_ELSE: {
              size_t newStart = refrences[--count];
              Token *elseToken = program->instructions[newStart];
              ControlFlowBlock *block = elseToken->data;
              if(block && block->program) break;
              block = elseToken->data = malloc(sizeof(ControlFlowBlock));
              Token *scope = getProgramInstruction(program, start - 1, true);
              block->program = scope->data;
              free(scope);
              i = newStart;

              block->endInstruction = 1;
              block->nextInstruction = 1;
              break;
            }
            default: break;
          }
        }

        break;
      }

      case TOKEN_IF: {
        refrences[count++] = i;
        break;
      }
      case TOKEN_ELSE: {
        if(i == 0 || program->instructions[i - 1]->type != TOKEN_IF) {
          fprintf(stderr, "ERROR: ");
          printTokenLocation(program->instructions[i], stderr);
          fprintf(stderr, ": Token `else` must follow an `if` token!\n");
          exit(ERROR_ELSE_AFTER_IF);
          return 1;
        }
        refrences[count++] = i;

        ControlFlowBlock *block = program->instructions[i - 1]->data;
        block->nextInstruction = (i) - (i - 1);
        size_t end = (i + 1) - (i - 1);
        block->endInstruction = end;
        for(size_t j = i - 1;j >= 1;j--) {
          Token *ifTok = program->instructions[j];
          if(ifTok->type != TOKEN_IF || j == 0) break;
          Token *beforeTok = program->instructions[j - 1];
          ControlFlowBlock *ifBlock = ifTok->data;
          ifBlock->endInstruction = (i + 1) - j;
          j--;
          if(beforeTok->type == TOKEN_IF) break;
          if(beforeTok->type == TOKEN_ELSE) {
            ControlFlowBlock *block = beforeTok->data;
            if(block && block->program) break;
          }
        }
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

  cleanupElseIfs(program);

  return 0;
}

void cleanupElseIfs(Program *program) {
  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];
    switch (instruction->type) {
      case TOKEN_ELSE: {
        ControlFlowBlock *block = instruction->data;
        if(!block || !block->program) {
          getProgramInstruction(program, i, true);
          for(long int j = i - 1;j >= 0;j--) {
            Token *prev = program->instructions[j];
            if(prev->type != TOKEN_IF) break;
            ControlFlowBlock *prevBlock = prev->data;
            if(!prevBlock || prevBlock->endInstruction <= 1) break;
            prevBlock->endInstruction--;
          }
          i--;
          break;
        }
        cleanupElseIfs(block->program);
        break;
      }
      case TOKEN_IF: {
        ControlFlowBlock *block = instruction->data;
        Program p = {.instructions = &block->condition, .count = 1};
        cleanupElseIfs(&p);
        cleanupElseIfs(block->program);
        
        break;
      }
      case TOKEN_PRIORITY: {
        TokenPriorityData *value = instruction->data;
        Program p = {.instructions = value->instructions, .count = value->count};
        cleanupElseIfs(&p);
        break;
      }
      case TOKEN_SCOPE: {
        cleanupElseIfs((Program*) instruction->data);
        break;
      }
      
      default:
        break;
    }

  }
}

bool shouldGoDeeper(TokenType type) {
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in shouldGoDeeper!");
  switch (type) {
    case TOKEN_PRIORITY:
    case TOKEN_SCOPE:
    case TOKEN_IF:
    case TOKEN_ELSE:
    case TOKEN_DECLARE_FUNCTION:
      return true;

    default:
      return false;
  }
}

void goDeeper(Token *token, goDeeperFunction fnc, int paramCount, ...) {
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in goDeeper!");
  Program *program = NULL, *program2 = NULL;
  Program prog;
  size_t *p_count = NULL;
  switch (token->type) {
    case TOKEN_PRIORITY: {
      TokenPriorityData *priorityData = token->data;

      prog.count = priorityData->count;
      prog.capacity = priorityData->count;
      prog.instructions = priorityData->instructions;
      prog.parent = NULL;
      prog.id = 0;
      prog.functions = NULL;
      program = &prog;
      p_count = &priorityData->count;
      break;
    }
    case TOKEN_SCOPE: {
      program = token->data;
      break;
    }
    case TOKEN_IF: {
      ControlFlowBlock *block = token->data;
      program = block->program;

      prog.count = 1;
      prog.capacity = 1;
      prog.instructions = &block->condition;
      prog.parent = NULL;
      prog.id = 0;
      prog.functions = NULL;

      program2 = &prog;
      break;
    }
    case TOKEN_ELSE: {
      ControlFlowBlock *block = token->data;
      program = block->program;
      break;
    }
    case TOKEN_DECLARE_FUNCTION: {
      FunctionDefinition *data = token->data;

      prog.count = data->parameters->count;
      prog.capacity = data->parameters->count;
      prog.instructions = data->parameters->instructions;
      prog.parent = NULL;
      prog.id = 0;
      prog.functions = NULL;
      program = &prog;
      p_count = &data->parameters->count;
      
      program2 = data->body;

      break;
    }
    default: {
      ASSERT(true, "Unreachable in `goDeeper`!");
      break;
    }
  }
  if(!program) {
    ASSERT(true, "Unreachable in `goDeeper`!");
    return;
  }
  
  while(true) {
    if(paramCount == 0) {
      (*fnc)(program);
      if(program == &prog && prog.count > 0 && p_count != NULL) {
        *p_count = program->count;
      }
    } else if(paramCount == 1) {
      va_list argp;
      va_start(argp, paramCount);
      (*fnc)(program, va_arg(argp, void*));
      va_end(argp);
    } else {
      fprintf(stderr, "ERROR: Unsupported `paramCount` at: %s:%zu:%zu\n",
            token->file, token->line, token->column);
      exit(-1);
    }
    if(program2) {
      program = program2;
      program2 = NULL;
      continue;
    }
    break;
  }
}

void crossreferenceFunctions(Program *program) {
  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];
    if(instruction->type != TOKEN_DECLARE_FUNCTION && shouldGoDeeper(instruction->type)) {
      goDeeper(instruction, (goDeeperFunction) crossreferenceFunctions, 0);
    }

    if(instruction->type != TOKEN_DECLARE_FUNCTION) {
      continue;
    }

    if(i == 0) {
      nameBeforeDeclareFunction:
      fprintf(stderr, "ERROR: %s at: %s:%zu:%zu\n",
            getPawscriptErrorName(ERROR_NAME_BEFORE_DECLARE_FUNCTION), instruction->file, instruction->line, instruction->column);
      exit(ERROR_NAME_BEFORE_DECLARE_FUNCTION);
      return;
    }
    Token *functionName = getProgramInstruction(program, --i, true);
    if(functionName->type != TOKEN_NAME) {
      goto nameBeforeDeclareFunction;
    }
    NameData *nameData = functionName->data;
    const char *name = nameData->variableName;

    size_t popIndex = i + 1;
    Token *functionParams = getProgramInstruction(program, popIndex, true);
    if(functionParams->type != TOKEN_PRIORITY) {
      fprintf(stderr, "ERROR: %s at: %s:%zu:%zu\n",
            getPawscriptErrorName(ERROR_PARAMS_AFTER_DECLARE_FUNCTION), instruction->file, instruction->line, instruction->column);
      exit(ERROR_PARAMS_AFTER_DECLARE_FUNCTION);
      return;
    }
    goDeeper(functionParams, (goDeeperFunction) crossreferenceFunctions, 0);

    Token *functionBody = getProgramInstruction(program, popIndex, true);
    if(functionBody->type == TOKEN_SCOPE) {
      goDeeper(functionBody, (goDeeperFunction) crossreferenceFunctions, 0);
      FunctionDefinition *function = malloc(sizeof(FunctionDefinition));
      function->name = name;
      function->body = functionBody->data;
      function->parameters = functionParams->data;
      function->returnType = TYPE_VOID;

      instruction->data = function;
      setElementInHashTable(program->functions, name, function);
      continue;
    }
    if(functionBody->type != TOKEN_ASSIGN_TYPE) {
      fprintf(stderr, "ERROR: %s at: %s:%zu:%zu\n",
            getPawscriptErrorName(ERROR_TYPE_AFTER_PARAMS_FUNCTION), instruction->file, instruction->line, instruction->column);
      exit(ERROR_TYPE_AFTER_PARAMS_FUNCTION);
      return;
    }
    Token *functionReturnType = getProgramInstruction(program, popIndex, true);
    if(functionReturnType->type != TOKEN_TYPE) {
      fprintf(stderr, "ERROR: %s at: %s:%zu:%zu\n",
            getPawscriptErrorName(ERROR_TYPE_AFTER_PARAMS_FUNCTION), instruction->file, instruction->line, instruction->column);
      exit(ERROR_TYPE_AFTER_PARAMS_FUNCTION);
      return;
    }
    functionBody = getProgramInstruction(program, popIndex, true);
    if(functionBody->type != TOKEN_SCOPE) {
      fprintf(stderr, "ERROR: %s at: %s:%zu:%zu\n",
            getPawscriptErrorName(ERROR_TYPE_AFTER_PARAMS_FUNCTION), instruction->file, instruction->line, instruction->column);
      exit(ERROR_TYPE_AFTER_PARAMS_FUNCTION);
      return;
    }
    goDeeper(functionBody, (goDeeperFunction) crossreferenceFunctions, 0);
    FunctionDefinition *function = malloc(sizeof(FunctionDefinition));
    function->name = name;
    function->body = functionBody->data;
    function->parameters = functionParams->data;
    function->returnType = (Type) functionReturnType->data;

    instruction->data = function;
    setElementInHashTable(program->functions, name, function);
  }
}

FunctionDefinition *getFunctionFromProgram(Program *program, const char *name) {
  FunctionDefinition *data = getElementFromHashTable(program->functions, name);
  if(data) return data;
  if(!program->parent) return NULL;
  return getFunctionFromProgram(program->parent, name);
}

NameMapValue *createAndAddNameMapVariable(HashTable *nameMap, const char *name, bool mutable, Program *program, size_t i) {
  NameMapValue *element = malloc(sizeof(NameMapValue));
  element->program = program;

  Type *type = malloc(sizeof(Type));
  *type = TYPE_NONE;
  element->type = type;

  element->mutable = mutable;

  const size_t newNameLength = 4 +
        (program->id == 0 ? 1 : ((int) log10(program->id) + 1)) +
        1 + (i == 0 ? 1 : ((int) log10(i) + 1)) + 1;
  char *newName = calloc(newNameLength, sizeof(char));
  snprintf(newName, newNameLength, "var_%zu_%zu", program->id, i);
  element->name = newName;

  setElementInHashTable(nameMap, name, element);

  return element;
}

void crossreferenceVariables(Program *program, HashTable *parentNameMap) {
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in crossreferenceVariables!");
  HashTable *nameMap = parentNameMap == NULL ? createHashTable(255) : createHashTableFrom(parentNameMap);
  for(size_t i = 0; i < program->count;i++) {
    Token *instruction = program->instructions[i];
    if(instruction->type == TOKEN_DECLARE_FUNCTION) {
      FunctionDefinition *data = instruction->data;
      ASSERT(data, "Unreachable!");
      TokenPriorityData *inputs = data->parameters;
      Program *body = data->body;
      Program inputsProgram = {
        .count = inputs->count,
        .capacity = inputs->count,
        .instructions = inputs->instructions,
        .parent = NULL,
        .id = 0,
        .functions = NULL,
      };
      for(size_t j = 0;j < inputs->count;j++) {
        Token *input = inputs->instructions[j];
        if(input->type != TOKEN_NAME) continue;
        NameData *inputName = input->data;

        if(j != 0) {
          Token *last = inputs->instructions[j - 1];
          if(last->type == TOKEN_MUT || last->type == TOKEN_CONST) {
            inputName->mutable = last->type == TOKEN_MUT;
            free(getProgramInstruction(&inputsProgram, j - 1, true));
            j--;
          }
        }

        NameMapValue *element = createAndAddNameMapVariable(nameMap, inputName->name, inputName->mutable, body, j);
        inputName->name    = element->name;
        inputName->type    = element->type;
        inputName->mutable = element->mutable;
      }

      crossreferenceVariables(body, nameMap);
      NameMapValue *functionElement = createAndAddNameMapVariable(nameMap, data->name, false, program, i);
      *functionElement->type = TYPE_FUNCTION;

      continue;
    } else if(shouldGoDeeper(instruction->type)) {
      goDeeper(instruction, (goDeeperFunction) crossreferenceVariables, 1, nameMap);
      continue;
    }
    if(instruction->type != TOKEN_NAME) continue;
    NameData *value = instruction->data;
    const char *name = value->name;

    bool assignType = false;
    if(i + 2 < program->count) {
      assignType = program->instructions[i + 1]->type == TOKEN_ASSIGN_TYPE && program->instructions[i + 2]->type == TOKEN_TYPE;
    }

    if(existsElementInHashTable(nameMap, name)) {
      NameMapValue *element = getElementFromHashTable(nameMap, name);
      if(!assignType || element->program == program) {
        value->name    = element->name;
        value->type    = element->type;
        value->mutable = element->mutable;
        continue;
      }
    }

    if(i == 0) {
      crossreferenceVariables_noMutabilityError_mainLoop:
      fprintf(stderr, "ERROR: %s: %s:%zu:%zu\n",
        getPawscriptErrorName(ERROR_NO_MUTABILITY_FOR_VARIABLE),
        instruction->file, instruction->line, instruction->column);
      exit(ERROR_NO_MUTABILITY_FOR_VARIABLE);
    }
    Token *last = program->instructions[i - 1];
    if(last->type != TOKEN_MUT && last->type != TOKEN_CONST) {
      goto crossreferenceVariables_noMutabilityError_mainLoop;
    }
    value->mutable = (last->type == TOKEN_MUT);
    free(getProgramInstruction(program, i - 1, true));
    i--;


    bool mutable = value->mutable;
    NameMapValue *element = createAndAddNameMapVariable(nameMap, name, mutable, program, i);

    value->name    = element->name;
    value->type    = element->type;
    value->mutable = element->mutable;
  }

  deleteHashTable(nameMap);
}

void typesetProgramError(PawscriptError pawscriptError, const char *variableName, Token *token) {
  fprintf(stderr, "ERROR: %s: Variable `%s` at %s:%zu:%zu\n",
    getPawscriptErrorName(pawscriptError),
    variableName,
    token->file, token->line, token->column);
  exit(pawscriptError);
  return;
}

void typesetProgramReassignError(const char *variableName, Token *token, Type expected, Type got) {
  fprintf(stderr, "ERROR: %s: Variable `%s` expected type `%s` got `%s` at %s:%zu:%zu\n",
    getPawscriptErrorName(ERROR_CANT_REASSIGN_VARIABLE_TYPE),
    variableName, getTypeName(expected), getTypeName(got),
    token->file, token->line, token->column);
  exit(ERROR_CANT_REASSIGN_VARIABLE_TYPE);
  return;
}

void typesetProgram(Program *program) {
  ASSERT(TOKEN_COUNT == 28, "Not all operations are implemented in typesetProgram!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in typesetProgram!");
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i], *next;
    if(shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) typesetProgram, 0);
      continue;
    }
    if(token->type != TOKEN_NAME) {
      continue;
    }
    NameData *value = token->data;

    if(i + 1 == program->count - 1 && program->instructions[i + 1]->type == TOKEN_ASSIGN_TYPE) {
      typesetProgramError(ERROR_NO_TYPE_AFTER_ASSIGN_TYPE, value->variableName, token);
      return;
    }

    if(i == program->count - 1) {
      if(value->type == TYPE_NONE) {
        typesetProgramError(ERROR_VARIABLE_NO_TYPE, value->variableName, token);
        return;
      } else {
        continue;
      }
    }
    next = program->instructions[i + 1];
    switch (next->type) {
      case TOKEN_ASSIGN_TYPE: {
        if(i == program->count - 2) {
          typesetProgramError(ERROR_NO_TYPE_AFTER_ASSIGN_TYPE, value->variableName, token);
          return;
        }
        next = program->instructions[i + 2];
        switch (next->type) {
          case TOKEN_TYPE: {
            Type type = (Type) next->data;
            if(*value->type == TYPE_NONE) {
              *value->type = type;
            } else if(*value->type != type) {
              typesetProgramError(ERROR_CANT_REASSIGN_VARIABLE_TYPE, value->variableName, token);
              return;
            }
            
            // Free the unneeded instructions (TOKEN_ASSIGN_TYPE and TOKEN_TYPE)
            free(getProgramInstruction(program, i + 1, true));
            free(getProgramInstruction(program, i + 1, true));
            break;
          }

          default: {
            typesetProgramError(ERROR_NO_TYPE_AFTER_ASSIGN_TYPE, value->variableName, token);
            return;
          }
        }
        break;
      }
      case TOKEN_ASSIGN: {
        if(i == program->count - 2) {
          typesetProgramError(ERROR_NO_ARGUMENT_AFTER_ASSIGN, value->variableName, token);
          return;
        }
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
              typesetProgramReassignError(value->variableName, token, *value->type, data->type);
              return;
            }
            break;
          }
          case TOKEN_NAME: {
            NameData *nextValue = next->data;
            if(*nextValue->type == TYPE_NONE) {
              typesetProgramError(ERROR_VARIABLE_NO_TYPE, nextValue->variableName, next);
              return;
            } else if(*value->type == TYPE_NONE) {
              *value->type = *nextValue->type;
              break;
            } else if(*value->type != *nextValue->type) {
              typesetProgramReassignError(value->variableName, token, *value->type, *nextValue->type);
              return;
            }
            break;
          }
          
          default: {
            typesetProgramError(ERROR_NO_ARGUMENT_AFTER_ASSIGN, value->variableName, token);
            return;
          }
        }
        break;
      }
      default: {
        if(value->type == TYPE_NONE) {
          typesetProgramError(ERROR_UNINITIALIZED_VARIABLE, value->variableName, token);
          return;
        } else {
          break;
        }
      }
    }
  }
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
        if(s == 0 && (end < length - 1 && !(isspace(line[end]) && line[end] != '\0'))) {
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

  crossreferenceBlocks(program);
  crossreferenceFunctions(program);
  crossreferenceVariables(program, NULL);
  typesetProgram(program);

  return program;
}

// Instructions

InstructionType INSTRUCTION_TYPES[] = {
  {.name = "=>",         .tokenType = TOKEN_DECLARE_FUNCTION,  .length = 2,  .fromRight = true },
  {.name = ":",          .tokenType = TOKEN_ASSIGN_TYPE,       .length = 1,  .fromRight = true },
  {.name = ";",          .tokenType = TOKEN_SEMICOLON,         .length = 1,  .fromRight = true },
  {.name = "=",          .tokenType = TOKEN_ASSIGN,            .length = 1,  .fromRight = true },
  {.name = "beta_print", .tokenType = TOKEN_PRINT,             .length = 10, .fromRight = false},
  {.name = "const",      .tokenType = TOKEN_CONST,             .length = 5,  .fromRight = false},
  {.name = "mut",        .tokenType = TOKEN_MUT,               .length = 3,  .fromRight = false},
  {.name = ",",          .tokenType = TOKEN_COMMA,             .length = 1,  .fromRight = true },
  {.name = "+",          .tokenType = TOKEN_ADD,               .length = 1,  .fromRight = true },
  {.name = "-",          .tokenType = TOKEN_SUBTRACT,          .length = 1,  .fromRight = true },
  {.name = ">",          .tokenType = TOKEN_GREATER_THAN,      .length = 1,  .fromRight = true },
  {.name = "<",          .tokenType = TOKEN_LESS_THAN,         .length = 1,  .fromRight = true },
  {.name = "(",          .tokenType = TOKEN_PARENTHESES_OPEN,  .length = 1,  .fromRight = true },
  {.name = ")",          .tokenType = TOKEN_PARENTHESES_CLOSE, .length = 1,  .fromRight = true },
  {.name = "{",          .tokenType = TOKEN_BRACES_OPEN,       .length = 1,  .fromRight = true },
  {.name = "}",          .tokenType = TOKEN_BRACES_CLOSE,      .length = 1,  .fromRight = true },
  {.name = "[",          .tokenType = TOKEN_BRACKETS_OPEN,     .length = 1,  .fromRight = true },
  {.name = "]",          .tokenType = TOKEN_BRACKETS_CLOSE,    .length = 1,  .fromRight = true },
  {.name = "if",         .tokenType = TOKEN_IF,                .length = 2,  .fromRight = false},
  {.name = "else",       .tokenType = TOKEN_ELSE,              .length = 4,  .fromRight = false},
  {.name = "==",         .tokenType = TOKEN_EQUALS,            .length = 2,  .fromRight = true },
  {.name = "!=",         .tokenType = TOKEN_NOT_EQUALS,        .length = 2,  .fromRight = true },
  {.name = "return",     .tokenType = TOKEN_RETURN,            .length = 6,  .fromRight = false},
  {.name = NULL,         .tokenType = -1,                      .length = 0,  .fromRight = false},
};
