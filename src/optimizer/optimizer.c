#include "optimizer.h"
#include "../utils/utils.h"
#include <time.h>

void removeVariableFromProgram(Program *program, NameData *variable) {
  List *variables = program->variables;
  bool startSwapping = false;
  for(size_t i = 0;i < variables->size;i++) {
    NameData *nd = variables->elements[i];
    if(strcmp(nd->name, variable->name) == 0) {
      variables->elements[i] = NULL;
      startSwapping = true;
      variables->size--;
    }
    if(startSwapping) {
      variables->elements[i] = variables->elements[i + 1];
    }
  }

  removeElementFromHashTable(program->variableMap, variable->name);
}

void optimizeConstVariables(Program *program, HashTable *constValues) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in optimizeConstVariables!");
  ASSERT(BASIC_TYPES_COUNT ==  5, "Not all types are implemented in optimizeConstVariables!");
  constValues = createHashTableFrom(constValues);

  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i], *next;
    if(token->type == TOKEN_PRINT) {
      Token *child = token->data;
      if(child->type != TOKEN_NAME) continue;
      NameData *nameData = child->data;
      if(!existsElementInHashTable(constValues, nameData->name)) {
        continue;
      }
      child->type = TOKEN_VALUE;
      child->data = getElementFromHashTable(constValues, nameData->name);
      free(nameData);
      continue;
    } else if(token->type == TOKEN_FUNCTION_CALL) {
      goDeeper(token, (goDeeperFunction) optimizeConstVariables, 1, constValues);
      FunctionCallData *callData = token->data;
      if(!callData || !callData->nameData) continue;
      if(!existsElementInHashTable(constValues, callData->nameData->name)) {
        continue;
      }
      ValueData *vd = getElementFromHashTable(constValues, callData->nameData->name);
      FunctionTypeData *dat = vd->data;

      callData->function = getFunctionFromProgram(program, dat->name);
      callData->nameData = NULL;
      continue;
    } else if(shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) optimizeConstVariables, 1, constValues);
      continue;
    }
    if(token->type != TOKEN_NAME) continue;
    NameData *nameData = token->data;

    if(existsElementInHashTable(constValues, nameData->name)) {
      program->instructions[i]->type = TOKEN_VALUE;
      program->instructions[i]->data = getElementFromHashTable(constValues, nameData->name);

      free(nameData);
      continue;
    }

    if(nameData->mutable) continue;
    if(i == program->count - 1) continue;
    next = program->instructions[i + 1];
    if(next->type != TOKEN_ASSIGN) continue;
    if(i == program->count - 2) continue;
    next = program->instructions[i + 2];
    switch (next->type) {
      case TOKEN_VALUE: {
        ValueData *valueData = next->data;
        switch(valueData->type.basicType) {
          case BASIC_TYPE_INT:
          case BASIC_TYPE_BOOL:
          case BASIC_TYPE_FUNCTION: {
            setElementInHashTable(constValues, nameData->name, valueData);

            removeVariableFromProgram(program, nameData);

            free(getProgramInstruction(program, i, true));
            free(getProgramInstruction(program, i, true));
            free(getProgramInstruction(program, i, true));
            i--;
            free(nameData);
            break;
          }
          default: {
            ASSERT(false, "Not all types are implemented in optimizeConstVariables!");
          }
        }
        break;
      }
      case TOKEN_NAME: {
        NameData *nextNameData = next->data;
        if(!existsElementInHashTable(constValues, nextNameData->name)) {
          break;
        }
        setElementInHashTable(constValues, nameData->name, getElementFromHashTable(constValues, nextNameData->name));

        removeVariableFromProgram(program, nameData);

        free(getProgramInstruction(program, i, true));
        free(getProgramInstruction(program, i, true));
        free(getProgramInstruction(program, i, true));
        i--;
        free(nameData);
        break;
      }
      default: {
        break;
      }
    }
  }

  if(!program->functions) return;
  HashTable *functions = program->functions;
  for(size_t i = 0;i < functions->capacity;i++) {
    if(!functions->elements[i].key) continue;
    FunctionDefinition *fd = functions->elements[i].value;
    optimizeConstVariables(fd->body, constValues);
  }
}

void cleanOffsets(Program *program) {
  program->variableOffset = 0;
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    if(token->type == TOKEN_DECLARE_FUNCTION) {
      FunctionDefinition *data = token->data;
      ASSERT(data, "Unreachable!");
      TokenPriorityData *inputs = data->parameters;
      for(size_t j = 0;j < inputs->count;j++) {
        Token *input = inputs->instructions[j];
        if(input->type != TOKEN_NAME) continue;
        NameData *inputName = input->data;
        ASSERT(inputName->offset != NULL, "Unreachable");
        *inputName->offset = 0;
      }

      cleanOffsets(data->body);
      continue;
    } else if(shouldGoDeeper(token->type)) {
      goDeeper(token, (goDeeperFunction) cleanOffsets, 0);
      continue;
    }
  }
  List *variables = program->variables;
  if(variables) {
    for(size_t i = 0;i < variables->size;i++) {
      NameData *variable = variables->elements[i];
      *variable->offset = 0;
    } 
  }

  if(!program->functions) return;
  HashTable *functions = program->functions;
  for(size_t i = 0;i < functions->capacity;i++) {
    if(!functions->elements[i].key) continue;
    FunctionDefinition *fd = functions->elements[i].value;
    cleanOffsets(fd->body);
  }
}

void recalculateOffsets(Program *program) {
  calculateOffsets(program);

  if(!program->functions) return;
  HashTable *functions = program->functions;
  for(size_t i = 0;i < functions->capacity;i++) {
    if(!functions->elements[i].key) continue;
    FunctionDefinition *data = functions->elements[i].value;

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

    recalculateOffsets(data->body);
  }
}

void optimizeProgram(Program *program) {
  clock_t startClock = clock(), optimizerStart = startClock;

  optimizeConstVariables(program, NULL);
  startClock = clock() - startClock;
  printf("[LOG]: [OPTIMIZER] Optimizing const variables   : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  cleanOffsets(program);
  startClock = clock() - startClock;
  printf("[LOG]: [OPTIMIZER] Cleaning offsets             : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  recalculateOffsets(program);
  startClock = clock() - startClock;
  printf("[LOG]: [OPTIMIZER] Recalculating offsets        : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  startClock = clock() - optimizerStart;
  printf("[LOG]: [OPTIMIZER] Optimizer                    : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
}
