#include "optimizer.h"
#include "../utils/utils.h"
#include <time.h>

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
        break;
      }
      child->type = TOKEN_VALUE;
      child->data = getElementFromHashTable(constValues, nameData->name);
      free(nameData);
    } else if(token->type == TOKEN_FUNCTION_CALL) {
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
    next = program->instructions[i + 1];
    if(next->type != TOKEN_ASSIGN) continue;
    next = program->instructions[i + 2];
    switch (next->type) {
      case TOKEN_VALUE: {
        ValueData *valueData = next->data;
        switch(valueData->type.basicType) {
          case BASIC_TYPE_INT:
          case BASIC_TYPE_BOOL:
          case BASIC_TYPE_FUNCTION: {
            setElementInHashTable(constValues, nameData->name, valueData);

            free(getProgramInstruction(program, i, true));
            free(getProgramInstruction(program, i, true));
            free(getProgramInstruction(program, i, true));
            i--;
            free(nameData);
            removeElementFromHashTable(program->variables, nameData->name);
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
        free(getProgramInstruction(program, i, true));
        free(getProgramInstruction(program, i, true));
        free(getProgramInstruction(program, i, true));
        i--;
        free(nameData);
        removeElementFromHashTable(program->variables, nameData->name);

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

void optimizeProgram(Program *program) {
  clock_t startClock = clock(), optimizerStart = startClock;

  optimizeConstVariables(program, NULL);
  startClock = clock() - startClock;
  printf("[LOG]: Optimizing const variables   : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  startClock = clock();

  startClock = clock() - optimizerStart;
  printf("[LOG]: Optimizer                    : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
}
