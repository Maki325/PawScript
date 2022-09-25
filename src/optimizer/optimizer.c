#include "optimizer.h"
#include <time.h>

void optimizeConstVariables(Program *program, HashTable *constValues) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in optimizeConstVariables!");
  ASSERT(TYPES_COUNT ==  5, "Not all types are implemented in optimizeConstVariables!");

  constValues = createHashTableFrom(constValues);

  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i], *next;
    if(shouldGoDeeper(token->type)) {
      goDeeper(token, optimizeConstVariables, 1, constValues);
      continue;
    }
    if(token->type != TOKEN_NAME) continue;
    NameData *nameData = token->data;
    if(nameData->mutable) continue;
    next = program->instructions[i + 1];
    if(next->type != TOKEN_ASSIGN) continue;
    next = program->instructions[i + 2];
    switch (next->type) {
      case TOKEN_VALUE: {
        ValueData *valueData = next->data;
        switch(valueData->type) {
          case TYPE_INT:
          case TYPE_BOOL:
          case TYPE_FUNCTION: {
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
    if(!isOperationTokenType(next->type)) {
      continue;
    }
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
