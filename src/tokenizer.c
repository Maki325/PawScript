#include "tokenizer.h"
#include "pawscript_error.h"
#include "utils/utils.h"
#include "config.h"
#include "platforms/linux_x86_64/tokenizer.h"
#include <stdarg.h>
#include <time.h>

size_t PROGRAM_COUNT = 0;

Program *createProgram() {
  Program *program = malloc(sizeof(Program));
  program->id = PROGRAM_COUNT++;
  program->parent = NULL;
  program->capacity = 20;
  program->count = 0;
  program->instructions = calloc(program->capacity, sizeof(Token*));
  program->functions = createHashTable(256);
  program->variables = createHashTable(256);
  program->variableOffset = 0;
  program->useInOffsetCalculations = true;

  return program;
}

Program *createProgramWithParent(Program *parent) {
  Program *program = createProgram();
  program->parent = parent;
  return program;
}

void deleteProgram(Program *program) {
  for(size_t i = 0;i < program->count;i++) {
    free(program->instructions[i]->data);
    free(program->instructions[i]);
  }
  free(program->instructions);
  deleteHashTable(program->functions);
  deleteHashTable(program->variables);
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
  data->functionType = NULL;

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
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in createTokenFromString!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in createTokenFromString!");
  Token *token = malloc(sizeof(Token));
  token->file = createOptions->file;
  token->line = createOptions->line;
  token->column = createOptions->column;
  token->wasInPriority = false;
  token->data = NULL;

  // 3 4 4 4 5
  if(strncmp("int", createOptions->string, max(createOptions->length, 3)) == 0) {
    createOptions->length -= 3;
    token->type = TOKEN_TYPE;
    token->data = (void*) TYPE_INT;
    return token;
  } else if(strncmp("bool", createOptions->string, max(createOptions->length, 4)) == 0) {
    createOptions->length -= 4;
    token->type = TOKEN_TYPE;
    token->data = (void*) TYPE_BOOL;
    return token;
  } else if(strncmp("void", createOptions->string, max(createOptions->length, 4)) == 0) {
    createOptions->length -= 4;
    token->type = TOKEN_TYPE;
    token->data = (void*) TYPE_VOID;
    return token;
  } else if(strncmp("true", createOptions->string, max(createOptions->length, 4)) == 0) {
    createOptions->length -= 4;
    token->type = TOKEN_VALUE;
    ValueData *value = malloc(sizeof(ValueData));
    value->type = TYPE_BOOL;
    value->data = malloc(sizeof(uint8_t));
    *((uint8_t*) value->data) = 1;
    token->data = value;
    return token;
  } else if(strncmp("false", createOptions->string, max(createOptions->length, 5)) == 0) {
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
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in isStringTokenFromRight!");
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
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in crossrefrenceProgram!");
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
          exitTokenError(ERROR_PARENTHESES_NOT_BALANCED, instruction);
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
        value->parent = program;
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
          exitTokenError(ERROR_BRACES_NOT_BALANCED, instruction);
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
                exitTokenError(ERROR_IF_NO_CONDITION, program->instructions[i]);
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
          exitTokenError(ERROR_ELSE_AFTER_IF, program->instructions[i]);
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

bool shouldGoDeeperBase(TokenType type) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in shouldGoDeeperBase!");
  switch (type) {
    case TOKEN_PRIORITY:
    case TOKEN_SCOPE:
    case TOKEN_IF:
    case TOKEN_ELSE:
    case TOKEN_DECLARE_FUNCTION:
    case TOKEN_FUNCTION_CALL:
    case TOKEN_RETURN:
      return true;

    default:
      return false;
  }
}

bool shouldGoDeeper(TokenType type) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in shouldGoDeeper!");
  return shouldGoDeeperBase(type) || isOperationTokenType(type);
}

void goDeeper(Token *token, goDeeperFunction fnc, int paramCount, ...) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in goDeeper!");
  Program *program = NULL, *program2 = NULL;
  Program prog;
  prog.id = 0;
  prog.parent = NULL;
  prog.variables = NULL;
  prog.functions = NULL;

  size_t *p_count = NULL;
  switch (token->type) {
    case TOKEN_PRIORITY: {
      TokenPriorityData *priorityData = token->data;

      prog.count = priorityData->count;
      prog.capacity = priorityData->count;
      prog.instructions = priorityData->instructions;
      prog.parent = priorityData->parent;
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
      program = &prog;
      p_count = &data->parameters->count;
      
      program2 = data->body;

      break;
    }
    case TOKEN_FUNCTION_CALL: {
      FunctionCallData *data = token->data;
      TokenPriorityData *priorityData = data->arguments;

      prog.count = priorityData->count;
      prog.capacity = priorityData->count;
      prog.instructions = priorityData->instructions;
      prog.parent = priorityData->parent;
      program = &prog;
      p_count = &priorityData->count;
      break;
    }
    case TOKEN_RETURN: {
      TokenPriorityData *priorityData = token->data;
      if(priorityData == NULL) {
        return;
      }

      prog.count = priorityData->count;
      prog.capacity = priorityData->count;
      prog.instructions = priorityData->instructions;
      prog.parent = priorityData->parent;
      program = &prog;
      p_count = &priorityData->count;
      break;
    }

    case TOKEN_ADD:
    case TOKEN_SUBTRACT:
    case TOKEN_GREATER_THAN:
    case TOKEN_LESS_THAN:
    case TOKEN_EQUALS:
    case TOKEN_NOT_EQUALS: {
      BinaryOperationData *data = token->data;
      return;
      if(!data) return;
      if(data->operandOne) {
        if(shouldGoDeeper(data->operandOne->type)) {
          if(paramCount == 0) {
            goDeeper(data->operandOne, fnc, paramCount);
          } else if(paramCount == 1) {
            va_list argp;
            va_start(argp, paramCount);
            goDeeper(data->operandOne, fnc, paramCount, va_arg(argp, void*));
            va_end(argp);
          } else {
            fprintf(stderr, "ERROR: Unsupported `paramCount` binary operation at: %s:%zu:%zu\n",
                  token->file, token->line, token->column);
            exit(-1);
          }
        } else {
          prog.count = 1;
          prog.capacity = 1;
          prog.instructions = &data->operandOne;
          program = &prog;
          p_count = &prog.count;
          break;
        }
      }
      if(data->operandTwo) {
        if(shouldGoDeeper(data->operandTwo->type)) {
          if(paramCount == 0) {
            goDeeper(data->operandTwo, fnc, paramCount);
          } else if(paramCount == 1) {
            va_list argp;
            va_start(argp, paramCount);
            goDeeper(data->operandTwo, fnc, paramCount, va_arg(argp, void*));
            va_end(argp);
          } else {
            fprintf(stderr, "ERROR: Unsupported `paramCount` binary operation at: %s:%zu:%zu\n",
                  token->file, token->line, token->column);
            exit(-1);
          }
        } else {
          prog.count = 1;
          prog.capacity = 1;
          prog.instructions = &data->operandTwo;
          p_count = &prog.count;
          if(program) {
            program2 = &prog;
          } else {
            program = &prog;
          }
          break;
        }
      }
      if(!program) {
        return;
      }
      break;
    }
    
    default: {
      ASSERT(false, "Unreachable in `goDeeper`!");
      break;
    }
  }
  if(!program) {
    ASSERT(false, "Unreachable in `goDeeper`!");
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
      exitTokenError(ERROR_NAME_BEFORE_DECLARE_FUNCTION, instruction);
      return;
    }
    Token *functionName = getProgramInstruction(program, --i, true);
    if(functionName->type != TOKEN_NAME) {
      exitTokenError(ERROR_NAME_BEFORE_DECLARE_FUNCTION, instruction);
      return;
    }
    NameData *nameData = functionName->data;
    const char *name = nameData->variableName;

    size_t popIndex = i + 1;
    Token *functionParams = getProgramInstruction(program, popIndex, true);
    if(functionParams->type != TOKEN_PRIORITY) {
      exitTokenError(ERROR_PARAMS_AFTER_DECLARE_FUNCTION, instruction);
      return;
    }
    goDeeper(functionParams, (goDeeperFunction) crossreferenceFunctions, 0);

    Token *functionBody = getProgramInstruction(program, popIndex, true);
    if(functionBody->type == TOKEN_SCOPE) {
      goDeeper(functionBody, (goDeeperFunction) crossreferenceFunctions, 0);
      FunctionDefinition *function = malloc(sizeof(FunctionDefinition));
      function->variableName = name;
      function->name = name;
      function->body = functionBody->data;
      function->parameters = functionParams->data;
      function->returnType = TYPE_VOID;
      function->isMain = strncmp(name, "main", 5) == 0;
      function->doesReturn = false;

      function->functionType = malloc(sizeof(FunctionType));

      function->functionType->outputSize = 1;
      function->functionType->output = calloc(1, sizeof(Type));
      function->functionType->output[0] = TYPE_VOID;

      function->functionType->inputSize = 0;
      for(size_t inc = 0;inc < function->parameters->count;inc++) {
        Token *tok = function->parameters->instructions[inc];
        if(tok->type != TOKEN_TYPE) continue;
        function->functionType->inputSize++;
      }
      function->functionType->input = calloc(function->functionType->inputSize, sizeof(Type));
      for(size_t inc = 0, it = 0;inc < function->parameters->count;inc++) {
        Token *tok = function->parameters->instructions[inc];
        if(tok->type != TOKEN_TYPE) continue;
        function->functionType->input[it++] = (Type) tok->data;
      }

      instruction->data = function;
      setElementInHashTable(program->functions, name, function);
      continue;
    }
    if(functionBody->type != TOKEN_ASSIGN_TYPE) {
      exitTokenError(ERROR_TYPE_AFTER_PARAMS_FUNCTION, instruction);
      return;
    }
    Token *functionReturnType = getProgramInstruction(program, popIndex, true);
    if(functionReturnType->type != TOKEN_TYPE) {
      exitTokenError(ERROR_TYPE_AFTER_PARAMS_FUNCTION, instruction);
      return;
    }
    functionBody = getProgramInstruction(program, popIndex, true);
    if(functionBody->type != TOKEN_SCOPE) {
      exitTokenError(ERROR_TYPE_AFTER_PARAMS_FUNCTION, instruction);
      return;
    }
    goDeeper(functionBody, (goDeeperFunction) crossreferenceFunctions, 0);
    FunctionDefinition *function = malloc(sizeof(FunctionDefinition));
    function->variableName = name;
    function->name = name;
    function->body = functionBody->data;
    function->parameters = functionParams->data;
    function->returnType = (Type) functionReturnType->data;
    function->isMain = strncmp(name, "main", 5) == 0;
    function->doesReturn = false;

    function->functionType = malloc(sizeof(FunctionType));

    function->functionType->outputSize = 1;
    function->functionType->output = calloc(1, sizeof(Type));
    function->functionType->output[0] = function->returnType;

    function->functionType->inputSize = 0;
    for(size_t inc = 0;inc < function->parameters->count;inc++) {
      Token *tok = function->parameters->instructions[inc];
      if(tok->type != TOKEN_TYPE) continue;
      function->functionType->inputSize++;
    }
    function->functionType->input = calloc(function->functionType->inputSize, sizeof(Type));
    for(size_t inc = 0, it = 0;inc < function->parameters->count;inc++) {
      Token *tok = function->parameters->instructions[inc];
      if(tok->type != TOKEN_TYPE) continue;
      function->functionType->input[it++] = (Type) tok->data;
    }

    instruction->data = function;
    setElementInHashTable(program->functions, name, function);
  }
}

NameMapValue *createAndAddNameMapVariable(HashTable *nameMap, NameData *nameData, FunctionType *functionType, Program *program, size_t i) {
  NameMapValue *element = malloc(sizeof(NameMapValue));
  element->program = program;

  Type *type = malloc(sizeof(Type));
  *type = TYPE_NONE;
  element->type = type;

  element->mutable = nameData->mutable;

  int32_t *offset = malloc(sizeof(int32_t));
  *offset = 0;
  element->offset = offset;

  element->functionType = functionType;

  const size_t newNameLength = 4 +
        (program->id == 0 ? 1 : ((int) log10(program->id) + 1)) +
        1 + (i == 0 ? 1 : ((int) log10(i) + 1)) + 1;
  char *newName = calloc(newNameLength, sizeof(char));
  snprintf(newName, newNameLength, "var_%zu_%zu", program->id, i);
  element->name = newName;

  setElementInHashTable(nameMap, nameData->name, element);
  setElementInHashTable(program->variables, newName, nameData);

  return element;
}

bool isOperationTokenType(TokenType type) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in isOperationTokenType!");
  return type == TOKEN_ADD || type == TOKEN_SUBTRACT
          || type == TOKEN_GREATER_THAN || type == TOKEN_LESS_THAN
          || type == TOKEN_EQUALS|| type == TOKEN_NOT_EQUALS;
}

void crossreferenceVariables(Program *program, HashTable *parentNameMap) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in crossreferenceVariables!");
  HashTable *nameMap = parentNameMap == NULL ? createHashTable(256) : createHashTableFrom(parentNameMap);
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
        if(input->type == TOKEN_COMMA) {
          free(getProgramInstruction(&inputsProgram, j, true));
          continue;
        }
        if(input->type != TOKEN_NAME) continue;
        NameData *inputName = input->data;

        if(j != 0) {
          Token *last = inputs->instructions[j - 1];
          if(last->type == TOKEN_MUT || last->type == TOKEN_CONST) {
            inputName->mutable = last->type == TOKEN_MUT;
            free(getProgramInstruction(&inputsProgram, j - 1, true));
            inputs->count = inputsProgram.count;
            j--;
          }
        }

        NameMapValue *element = createAndAddNameMapVariable(nameMap, inputName, calloc(1, sizeof(FunctionType)), body, j);
        inputName->name               = element->name;
        inputName->type               = element->type;
        inputName->mutable            = element->mutable;
        inputName->offset             = element->offset;
        inputName->functionType       = element->functionType;

        if(j != inputs->count - 1) {
          Token *next = inputs->instructions[j + 1];
          if(next->type != TOKEN_ASSIGN_TYPE) {
            exitTokenError(ERROR_VARIABLE_NO_TYPE, input);
          }
          next = inputs->instructions[j + 2];
          if(next->type != TOKEN_TYPE) {
            exitTokenError(ERROR_VARIABLE_NO_TYPE, input);
          }
          *element->type = (Type) next->data;
          free(getProgramInstruction(&inputsProgram, j + 2, true));
          free(getProgramInstruction(&inputsProgram, j + 1, true));
          inputs->count = inputsProgram.count;
        }
      }

      crossreferenceVariables(body, nameMap);
      NameData *functionName = malloc(sizeof(NameData));
      functionName->mutable = false;
      functionName->name = data->name;
      functionName->variableName = data->variableName;
      functionName->functionType = NULL;
      NameMapValue *functionElement = createAndAddNameMapVariable(nameMap, functionName, data->functionType, program, i);
      *functionElement->type = TYPE_FUNCTION;
      functionName->name         = functionElement->name;
      functionName->type         = functionElement->type;
      functionName->offset       = functionElement->offset;
      functionName->mutable      = functionElement->mutable;
      functionName->functionType = functionElement->functionType;

      // TIP: Moving the function from variable name to generated name
      FunctionDefinition *function = removeElementFromHashTable(program->functions, functionName->variableName);
      function->name = functionName->name;
      setElementInHashTable(program->functions, functionName->name, function);

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

    bool isMutConst = false;
    if(i > 0) {
      Token *last = program->instructions[i - 1];
      isMutConst = last->type == TOKEN_MUT || last->type == TOKEN_CONST;
    }

    if(existsElementInHashTable(nameMap, name)) {
      NameMapValue *element = getElementFromHashTable(nameMap, name);
      if(!isMutConst && (!assignType || element->program == program)) {
        value->name          = element->name;
        value->type          = element->type;
        value->mutable       = element->mutable;
        value->offset        = element->offset;
        value->functionType  = element->functionType;
        continue;
      }
    }

    if(i == 0) {
      exitTokenError(ERROR_UNDECLARED_VARIABLE, instruction);
      return;
    }
    Token *last = program->instructions[i - 1];
    if(last->type != TOKEN_MUT && last->type != TOKEN_CONST) {
      exitTokenError(ERROR_UNDECLARED_VARIABLE, instruction);
      return;
    }
    value->mutable = (last->type == TOKEN_MUT);
    free(getProgramInstruction(program, i - 1, true));
    i--;

    NameMapValue *element = createAndAddNameMapVariable(nameMap, value, calloc(1, sizeof(FunctionType)), program, i);
    value->name               = element->name;
    value->type               = element->type;
    value->mutable            = element->mutable;
    value->offset             = element->offset;
    value->functionType       = element->functionType;
  }

  deleteHashTable(nameMap);
}

void removeUnneededPriorities(Program *program) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in removeUnneededPriorities!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in removeUnneededPriorities!");
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    if(token->type != TOKEN_PRIORITY) {
      if(shouldGoDeeper(token->type)) {
        goDeeper(token, (goDeeperFunction) removeUnneededPriorities, 0);
      }
      continue;
    }
    TokenPriorityData *data = token->data;
    if(data->count == 1) {
      program->instructions[i] = data->instructions[0];
      program->instructions[i]->wasInPriority = true;
      free(data->instructions);
      free(data);
      free(token);
    } else {
      goDeeper(token, (goDeeperFunction) removeUnneededPriorities, 0);
    }
  }
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

void typesetProgramReturnTypeError(Token *returnToken, Type expected, Type returnType) {
  fprintf(stderr, "ERROR: %s: Function type: `%s`, return type `%s` at %s:%zu:%zu\n",
    getPawscriptErrorName(ERROR_RETURN_TYPE_NOT_MATCHING),
    getTypeName(expected), getTypeName(returnType),
    returnToken->file, returnToken->line, returnToken->column);
  exit(ERROR_RETURN_TYPE_NOT_MATCHING);
  return;
}

void typesetProgramReturnTypeVoidFunctionError(Token *returnToken) {
  TokenPriorityData *priorityData = returnToken->data;
  Token *instruction = priorityData->instructions[0];
  switch(instruction->type) {
    case TOKEN_NAME: {
      NameData *nameData = instruction->data;
      typesetProgramReturnTypeError(returnToken, TYPE_VOID, *nameData->type);
      return;
    }
    case TOKEN_VALUE: {
      ValueData *valueData = instruction->data;
      typesetProgramReturnTypeError(returnToken, TYPE_VOID, valueData->type);
      return;
    }
    default: {
      ASSERT(false, "Unknown type!");
    }
  }
}

void typesetProgram(Program *program) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in typesetProgram!");
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
              if(*value->type != TYPE_FUNCTION) {
                break;
              }
              FunctionTypeData *functionTypeData = data->data;
              ASSERT(value->functionType, "Unreachable!");
              value->functionType->inputSize  = functionTypeData->functionType->inputSize;
              value->functionType->input      = functionTypeData->functionType->input;
              value->functionType->outputSize = functionTypeData->functionType->outputSize;
              value->functionType->output     = functionTypeData->functionType->output;
              break;
            } else if(*value->type != data->type) {
              // TODO: Add multiple types support for value token
              typesetProgramReassignError(value->variableName, token, *value->type, data->type);
              return;
            } else if(*value->type == TYPE_FUNCTION) {
              FunctionTypeData *ftd = data->data;
              if(value->functionType == ftd->functionType) {
                break;
              }
              if(
                !value->functionType || !ftd->functionType ||
                value->functionType->inputSize != ftd->functionType->inputSize ||
                value->functionType->outputSize != ftd->functionType->outputSize
              ) {
                typesetProgramReassignError(value->variableName, token, *value->type, data->type);
                return;
              }
              for(size_t index = 0;index < value->functionType->inputSize;index++) {
                if(value->functionType->input[index] != ftd->functionType->input[index]) {
                  typesetProgramReassignError(value->variableName, token, *value->type, data->type);
                  return;
                }
              }
              for(size_t index = 0;index < value->functionType->outputSize;index++) {
                if(value->functionType->output[index] != ftd->functionType->output[index]) {
                  typesetProgramReassignError(value->variableName, token, *value->type, data->type);
                  return;
                }
              }
              break;
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

          case TOKEN_ADD:
          case TOKEN_SUBTRACT:
          case TOKEN_GREATER_THAN:
          case TOKEN_LESS_THAN:
          case TOKEN_NOT_EQUALS:
          case TOKEN_EQUALS: {
            BinaryOperationData *data = next->data;
            if(data->type == TYPE_NONE) {
              exitTokenError(ERROR_OPERATION_NO_TYPE, next);
              return;
            } else if(*value->type == TYPE_NONE) {
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
          
          case TOKEN_FUNCTION_CALL: {
            FunctionCallData *nextData = next->data;
            if(*value->type == TYPE_NONE) {
              // TODO: Add multiple types support for function token
              if(nextData->function) {
                *value->type = nextData->function->returnType;
              } else if(nextData->nameData) {
                *value->type = nextData->nameData->functionType->output[0];
              }
              break;
            }
            if(nextData->function) {
              if(*value->type != nextData->function->returnType) {
                // TODO: Add multiple types support for function token
                typesetProgramReassignError(value->variableName, token, *value->type, nextData->function->returnType);
                return;
              }
            } else if(nextData->nameData) {
              if(*value->type != nextData->nameData->functionType->output[0]) {
                // TODO: Add multiple types support for function token
                typesetProgramReassignError(value->variableName, token, *value->type, nextData->nameData->functionType->output[0]);
                return;
              }
            }
            break;
          }

          default: {
            printToken(next, 0, i);
            typesetProgramError(ERROR_UNKNOWN_ARGUMENT_AFTER_ASSIGN, value->variableName, token);
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

void checkReturns(Program *program, FunctionDefinition *functionDefinition) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in typesetProgram!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in typesetProgram!");
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    if(token->type == TOKEN_DECLARE_FUNCTION) {
      goDeeper(token, (goDeeperFunction) checkReturns, 1, token->data);
      continue;
    }
    if(token->type != TOKEN_RETURN && shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) checkReturns, 1, functionDefinition, 0);
      continue;
    }
    if(token->type != TOKEN_RETURN) {
      continue;
    }
    
    TokenPriorityData *priorityData = token->data;
    if(!functionDefinition) {
      if(priorityData->count == 0) {
        goDeeper(token, (goDeeperFunction) typesetProgram, 1, NULL);
        continue;
      }

      typesetProgramReturnTypeVoidFunctionError(token);

      return;
    }
    Type returnType = functionDefinition->returnType;
    if(returnType == TYPE_VOID && priorityData->count != 0) {
      typesetProgramReturnTypeVoidFunctionError(token);
      return;
    }
    if(priorityData->count == 0) {
      if(returnType != TYPE_VOID) {
        typesetProgramReturnTypeError(token, returnType, TYPE_VOID);
      } else {
        functionDefinition->doesReturn = true;
        goDeeper(token, (goDeeperFunction) typesetProgram, 1, functionDefinition);
        continue;
      }
    }
    if(priorityData->count != 1) {
      ASSERT(false, "Add multi-type return!");
    }
    Token *instruction = priorityData->instructions[0];
    if(isOperationTokenType(instruction->type)) {
      BinaryOperationData *bod = instruction->data;
      if(bod->type == returnType) {
        functionDefinition->doesReturn = true;
        goDeeper(token, (goDeeperFunction) typesetProgram, 1, functionDefinition);
        continue;
      }
      exitTokenError(ERROR_RETURN_TYPE_NOT_MATCHING, token);
      return;
    }
    switch(instruction->type) {
      case TOKEN_NAME: {
        NameData *nameData = instruction->data;
        if(*nameData->type == returnType) {
          break;
        }
        exitTokenError(ERROR_RETURN_TYPE_NOT_MATCHING, token);
        return;
      }
      case TOKEN_VALUE: {
        ValueData *valueData = instruction->data;
        if(valueData->type == returnType) {
          break;
        }
        exitTokenError(ERROR_RETURN_TYPE_NOT_MATCHING, token);
        return;
      }
      case TOKEN_FUNCTION_CALL: {
        FunctionCallData *fcd = instruction->data;
        if(getFunctionReturnTypeFromCall(fcd) == returnType) {
          break;
        }
        exitTokenError(ERROR_RETURN_TYPE_NOT_MATCHING, token);
        return;
      }
      default: {
        ASSERT(false, "Unknown type!");
      }
    }
    
    functionDefinition->doesReturn = true;
    goDeeper(token, (goDeeperFunction) checkReturns, 1, functionDefinition);
    continue;
  }

  if(functionDefinition &&
    program == functionDefinition->body &&
    functionDefinition->returnType != TYPE_VOID &&
    !functionDefinition->doesReturn) {
    if(program->count != 0) {
      exitTokenError(ERROR_NO_RETURN, program->instructions[program->count - 1]);
    } else {
      exitError(ERROR_NO_RETURN);
    }
  }
}

size_t getTypeByteSize(Type type) {
  switch(config.platform) {
    case PLATFORM_LINUX_x86_64: {
      return getTypeByteSize_linux_x86_64(type);
    }
    default: {
      exitError(ERROR_PLATFORM_NOT_SUPPORTED);
      return 0;
    }
  }
}

void calculateOffsets(Program *program) {
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    if(token->type == TOKEN_DECLARE_FUNCTION) {
      FunctionDefinition *data = token->data;
      ASSERT(data, "Unreachable!");
      TokenPriorityData *inputs = data->parameters;
      int32_t offset = 8;
      for(size_t j = 0;j < inputs->count;j++) {
        Token *input = inputs->instructions[j];
        if(input->type != TOKEN_NAME) continue;
        NameData *inputName = input->data;
        ASSERT(inputName->offset != NULL, "Unreachable");
        *inputName->offset = offset + getTypeByteSize(*inputName->type);
        offset = *inputName->offset;
      }

      calculateOffsets(data->body);
      continue;
    } else if(shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) calculateOffsets, 0);
      continue;
    }
  }
  HashTable *variables = program->variables;
  if(!variables) return;
  for(size_t i = 0;i < variables->capacity;i++) {
    if(variables->elements[i].key == NULL) continue;
    NameData *variable = variables->elements[i].value;
    if(*variable->offset != 0) continue;
    *variable->offset = program->variableOffset - getTypeByteSize(*variable->type);
    program->variableOffset = *variable->offset;
  }
}

bool canBeUsedInArithmeticOperations(TokenType type) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in canBeUsedInArithmeticOperations!");
  return type == TOKEN_NAME || type == TOKEN_VALUE || type == TOKEN_ADD
          || type == TOKEN_SUBTRACT || type == TOKEN_PRIORITY;
}
bool canBeUsedInComparisonOperations(TokenType type) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in canBeUsedInComparisonOperations!");
  return type == TOKEN_NAME || type == TOKEN_VALUE || type == TOKEN_ADD
        || type == TOKEN_SUBTRACT || type == TOKEN_PRIORITY
        || type == TOKEN_GREATER_THAN || type == TOKEN_LESS_THAN;
}

void crossreferenceOperations(Program *program) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in typesetProgram!");
  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];

    switch(instruction->type) {
      case TOKEN_ADD:
      case TOKEN_SUBTRACT: {
        if(instruction->data) {
          BinaryOperationData *value = instruction->data;
          if(value->operandOne && value->operandTwo) break;
          else if(value->operandOne || value->operandTwo) {
            exitTokenError(ERROR_OPERATION_DOESNT_HAVE_BOTH_OPERANDS, instruction);
            return;
          }
        }
        if(i == 0 || i == program->count - 1) {
          exitTokenError(ERROR_OPERATION_NOT_ENOUGH_OPERANDS, instruction);
          return;
        }

        Token *left  = getProgramInstruction(program, i - 1, false),
              *right = getProgramInstruction(program, i + 1, false);
        if(!canBeUsedInArithmeticOperations(left->type)) {
          exitTokenError(ERROR_OPERAND_CANT_BE_USED, left);
          return;
        }
        if(!canBeUsedInArithmeticOperations(right->type)) {
          exitTokenError(ERROR_OPERAND_CANT_BE_USED, right);
          return;
        }
        // NOTE: Since the crossrefrences are done above
        // No need to redo the left one? and only do the right one
        // if(left->type == TOKEN_PRIORITY) {
        //   goDeeper(program->instructions[i - 1], (goDeeperFunction) crossreferenceOperations, 0);
        // }

        if(right->type == TOKEN_PRIORITY) {
          goDeeper(program->instructions[i + 1], (goDeeperFunction) crossreferenceOperations, 0);
        }
        BinaryOperationData *value = instruction->data = malloc(sizeof(BinaryOperationData));

        value->operandTwo = getProgramInstruction(program, i + 1, true);
        value->operandOne = getProgramInstruction(program, i - 1, true);
        i -= 1;

        // [FUTURE]: Actually check what the data is!!!
        value->type = TYPE_INT;

        break;
      }
    
      default: break;
    }
    if(shouldGoDeeper(instruction->type)) {
      goDeeper(instruction, (goDeeperFunction) crossreferenceOperations, 0);
      continue;
    }
  }

  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];

    switch(instruction->type) {
      case TOKEN_GREATER_THAN:
      case TOKEN_LESS_THAN: {
        if(i == 0 || i == program->count - 1) {
          exitTokenError(ERROR_OPERATION_NOT_ENOUGH_OPERANDS, instruction);
          return;
        }

        Token *left  = getProgramInstruction(program, i - 1, false),
              *right = getProgramInstruction(program, i + 1, false);
        if(!canBeUsedInComparisonOperations(left->type)) {
          exitTokenError(ERROR_OPERAND_CANT_BE_USED, left);
          return;
        }
        if(!canBeUsedInComparisonOperations(right->type)) {
          exitTokenError(ERROR_OPERAND_CANT_BE_USED, right);
          return;
        }
        
        // NOTE: Since the crossrefrences are done above
        // No need to redo them?
        BinaryOperationData *value = instruction->data = malloc(sizeof(BinaryOperationData));
        value->operandTwo = getProgramInstruction(program, i + 1, true);
        value->operandOne = getProgramInstruction(program, i - 1, true);
        i -= 1;

        // [FUTURE]: Actually check what the data is!!!
        value->type = TYPE_INT;

        break;
      }

      default: break;
    }
  }

  for(size_t i = 0;i < program->count;i++) {
    Token *instruction = program->instructions[i];

    switch(instruction->type) {
      case TOKEN_PRINT: {
        if(i == program->count - 1) {
          exitTokenError(ERROR_OPERATION_NOT_ENOUGH_OPERANDS, instruction);
          return;
        }

        Token *right = getProgramInstruction(program, i + 1, true);
        NameData *value = right->data;
        free(right);
        instruction->data = (void*) value->name;
        free(value);

        break;
      }
      case TOKEN_RETURN: {
        if(i == program->count - 1) {
          exitTokenError(ERROR_OPERATION_NOT_ENOUGH_OPERANDS, instruction);
          return;
        }

        size_t j = i + 1, len = 0, valueCount = 0;
        while(j < program->count && program->instructions[j]->type != TOKEN_SEMICOLON) {
          if(canBeUsedInComparisonOperations(program->instructions[j]->type)) valueCount++;
          len++;
          j++;
        }
        TokenPriorityData *data = malloc(sizeof(TokenPriorityData));
        data->parent = program;
        data->instructions = calloc(valueCount, sizeof(Token *));
        data->count = valueCount;

        for(size_t j = 0, index = 0;j < len;j++) {
          size_t calcIndex = i + 1 + j - index;
          if(canBeUsedInComparisonOperations(program->instructions[calcIndex]->type)) {
            data->instructions[index] = getProgramInstruction(program, calcIndex, true);
            index++;
          }
        }
        instruction->data = data;

        break;
      }

      case TOKEN_NOT_EQUALS:
      case TOKEN_EQUALS: {
        if(i == 0 || i == program->count - 1) {
          exitTokenError(ERROR_OPERATION_NOT_ENOUGH_OPERANDS, instruction);
          return;
        }

        Token *left  = getProgramInstruction(program, i - 1, false),
              *right = getProgramInstruction(program, i + 1, false);
        if(!canBeUsedInComparisonOperations(left->type)) {
          exitTokenError(ERROR_OPERAND_CANT_BE_USED, left);
          return;
        }
        if(!canBeUsedInComparisonOperations(right->type)) {
          exitTokenError(ERROR_OPERAND_CANT_BE_USED, right);
          return;
        }
        
        // NOTE: Since the crossrefrences are done above
        // No need to redo them?
        BinaryOperationData *value = instruction->data = malloc(sizeof(BinaryOperationData));
        value->operandTwo = getProgramInstruction(program, i + 1, true);
        value->operandOne = getProgramInstruction(program, i - 1, true);
        i -= 1;

        // [FUTURE]: Actually check what the data is!!!
        value->type = TYPE_BOOL;

        break;
      }

      default: break;
    }
  }
}

void removeFunctionTokens(Program *program) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in typesetProgram!");
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    if(token->type == TOKEN_DECLARE_FUNCTION) {
      goDeeper(token, (goDeeperFunction) removeFunctionTokens, 0);
      free(getProgramInstruction(program, i, true));
      i--;
      continue;
    } else if(shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) removeFunctionTokens, 0);
      continue;
    }
  }
}

FunctionDefinition *getFunctionFromProgram(Program *program, const char *name) {
  if(program->functions && existsElementInHashTable(program->functions, name)) {
    FunctionDefinition *data = getElementFromHashTable(program->functions, name);
    if(data) return data;
  }
  if(!program->parent) return NULL;
  return getFunctionFromProgram(program->parent, name);
}

void createFunctionCalls(Program *program) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in removeUnneededPriorities!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in removeUnneededPriorities!");
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    if(shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) createFunctionCalls, 0);
      continue;
    }
    if(token->type != TOKEN_VALUE && token->type != TOKEN_NAME) {
      continue;
    }
    if(token->type == TOKEN_VALUE) {
      ValueData *valueData = token->data;
      if(valueData->type != TYPE_FUNCTION) continue;
    }
    if(i + 1 == program->count) {
      continue;
    }
    Token *next = program->instructions[i + 1];
    if(next->type != TOKEN_PRIORITY && !next->wasInPriority) {
      continue;
    }

    if(token->type == TOKEN_VALUE) {
      ValueData *valueData = token->data;
      FunctionTypeData *functionTypeData = valueData->data;
      FunctionDefinition* function = getFunctionFromProgram(program, functionTypeData->name);

      FunctionCallData *functionCallData = malloc(sizeof(FunctionCallData));
      functionCallData->function = function;
      functionCallData->nameData = NULL;
      if(next->type == TOKEN_PRIORITY) {
        TokenPriorityData *priorityData = next->data;
        for(size_t arg = 0;arg < priorityData->count;arg++) {
          Token *argument = priorityData->instructions[arg];
          if(argument->type == TOKEN_COMMA) {
            Program p = {
              .capacity = priorityData->count,
              .count = priorityData->count,
              .instructions = priorityData->instructions,
            };
            free(getProgramInstruction(&p, arg--, true));
            priorityData->count--;
            continue;
          } else if(isOperationTokenType(argument->type)) {
            continue;
          }
          switch(argument->type) {
            case TOKEN_VALUE: continue;
            case TOKEN_NAME: continue;
            default: {
              exitTokenError(ERROR_UNKNOWN_TOKEN_IN_FUNCTION_CALL, argument);
              return;
            }
          }
          return;
        }

        functionCallData->arguments = priorityData;
        if(functionCallData->function->parameters->count != priorityData->count) {
          exitTokenError(ERROR_FUNCTION_CALL_ARGUMENTS_LENGTH_MISMATCH, next);
          return;
        }
        for(size_t j = 0;j < priorityData->count;j++) {
          // TODO: check types of all the arguments
        }
        free(getProgramInstruction(program, i + 1, true));
        program->instructions[i]->type = TOKEN_FUNCTION_CALL;
        program->instructions[i]->data = functionCallData;

        continue;
      }
      if(functionCallData->function->parameters->count != 1) {
        exitTokenError(ERROR_FUNCTION_CALL_ARGUMENTS_LENGTH_MISMATCH, next);
        return;
      }
      TokenPriorityData *priorityData = functionCallData->arguments = calloc(1, sizeof(TokenPriorityData));
      priorityData->parent = program;
      priorityData->count = 1;
      priorityData->instructions = calloc(1, sizeof(Token*));
      priorityData->instructions[0] = getProgramInstruction(program, i + 1, true);

      // TODO: check types of the single argument
      
      program->instructions[i]->type = TOKEN_FUNCTION_CALL;
      program->instructions[i]->data = functionCallData;

      continue;
    }

    NameData *nameData = token->data;

    FunctionCallData *functionCallData = malloc(sizeof(FunctionCallData));
    functionCallData->function = NULL;
    functionCallData->nameData = nameData;
    if(next->type == TOKEN_PRIORITY) {
      TokenPriorityData *priorityData = next->data;
      for(size_t arg = 0;arg < priorityData->count;arg++) {
        Token *argument = priorityData->instructions[arg];
        if(argument->type == TOKEN_COMMA) {
          Program p = {
            .capacity = priorityData->count,
            .count = priorityData->count,
            .instructions = priorityData->instructions,
          };
          free(getProgramInstruction(&p, arg--, true));
          priorityData->count--;
          continue;
        } else if(isOperationTokenType(argument->type)) {
          continue;
        }
        switch(argument->type) {
          case TOKEN_VALUE: continue;
          case TOKEN_NAME: continue;
          default: {
            exitTokenError(ERROR_UNKNOWN_TOKEN_IN_FUNCTION_CALL, argument);
            return;
          }
        }
        return;
      }

      functionCallData->arguments = priorityData;
      // TODO: check lenght, and types of all the arguments
      free(getProgramInstruction(program, i + 1, true));
      program->instructions[i]->type = TOKEN_FUNCTION_CALL;
      program->instructions[i]->data = functionCallData;
      continue;
    }
    TokenPriorityData *priorityData = functionCallData->arguments = calloc(1, sizeof(TokenPriorityData));
    priorityData->parent = program;
    priorityData->count = 1;
    priorityData->instructions = calloc(1, sizeof(Token*));
    priorityData->instructions[0] = getProgramInstruction(program, i + 1, true);

    // TODO: check types of the single argument
    program->instructions[i]->type = TOKEN_FUNCTION_CALL;
    program->instructions[i]->data = functionCallData;

    continue;
  }

  HashTable *functions = program->functions;
  if(!functions) return;
  for(size_t i = 0;i < functions->capacity;i++) {
    if(functions->elements[i].key == NULL) continue;
    FunctionDefinition *data = functions->elements[i].value;
    if(data->body) {
      createFunctionCalls(data->body);
    }
  }
}

void fixFunctionVariables(Program *program) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in fixFunctionVariables!");
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    if(shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) fixFunctionVariables, 0);
      continue;
    }
    if(token->type != TOKEN_NAME) continue;
    NameData *nameData = token->data;
    if(*nameData->type != TYPE_FUNCTION) continue;

    program->instructions[i]->type = TOKEN_VALUE;
    ValueData *valueData = malloc(sizeof(ValueData));
    valueData->type = TYPE_FUNCTION;
    FunctionTypeData *functionTypeData = valueData->data = malloc(sizeof(FunctionTypeData));
    functionTypeData->name = nameData->name;
    functionTypeData->functionType = nameData->functionType;
    program->instructions[i]->data = valueData;

    free(nameData);
  }
}

Program *createProgramFromFile(const char *filePath, char *error) {
  clock_t startClock = clock();

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
    size_t column = 0;
    row++;
    length = (size_t) lineLength;
    char *line = lineStart;
    trimRight(line, &length);
    if(length == 0) continue;
    column += trimLeft(&line, &length);
    for(size_t st = 0; st < length;st++) {
      if(line[st] == '/' && line[st - 1] == '/') {
        length = st - 1;
        break;
      }
    }
    column += trimLeft(&line, &length);
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
      createOptions.column = column + start + 1;
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
  startClock = clock() - startClock;
  printf("[LOG]: File reading & tokenizing    : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  crossreferenceBlocks(program);
  startClock = clock() - startClock;
  printf("[LOG]: Crossreferencing blocks      : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  crossreferenceFunctions(program);
  startClock = clock() - startClock;
  printf("[LOG]: Crossreferencing functions   : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  crossreferenceVariables(program, NULL);
  startClock = clock() - startClock;
  printf("[LOG]: Crossreferencing variables   : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  crossreferenceOperations(program);
  startClock = clock() - startClock;
  printf("[LOG]: Crossreferencing operations  : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  fixFunctionVariables(program);
  startClock = clock() - startClock;
  printf("[LOG]: Fixing Function Variables    : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  removeUnneededPriorities(program);
  startClock = clock() - startClock;
  printf("[LOG]: Removing Unneeded Priorities : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  createFunctionCalls(program);
  startClock = clock() - startClock;
  printf("[LOG]: Creating function calls      : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  typesetProgram(program);
  startClock = clock() - startClock;
  printf("[LOG]: Typeseting                   : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  checkReturns(program, NULL);
  startClock = clock() - startClock;
  printf("[LOG]: Checking returns             : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  calculateOffsets(program);
  startClock = clock() - startClock;
  printf("[LOG]: Calculating offsets          : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  removeFunctionTokens(program);
  startClock = clock() - startClock;
  printf("[LOG]: Removing function tokens     : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  return program;
}

// Instructions

InstructionType INSTRUCTION_TYPES[] = {
  {.name = "==",         .tokenType = TOKEN_EQUALS,            .length = 2,  .fromRight = true },
  {.name = "!=",         .tokenType = TOKEN_NOT_EQUALS,        .length = 2,  .fromRight = true },
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
  {.name = "return",     .tokenType = TOKEN_RETURN,            .length = 6,  .fromRight = false},
  {.name = NULL,         .tokenType = -1,                      .length = 0,  .fromRight = false},
};
