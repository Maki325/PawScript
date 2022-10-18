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

ValueData *convertValueData(ValueData *oldValueData, Type newType) {
  Type oldType = oldValueData->type;
  ValueData *valueData = malloc(sizeof(ValueData));
  valueData->type = newType;

  if(areTypesEqual(&oldType, &newType)) {
    valueData->data = oldValueData->data;
    return valueData;
  }

  switch (newType.basicType) {
    case BASIC_TYPE_BOOL: {
      switch (oldType.basicType) {
        case BASIC_TYPE_INT: {
          uint8_t *value = malloc(sizeof(uint8_t));
          *value = getNormalizedBoolValueFromUInt64(oldValueData->data);
          valueData->data = value;

          return valueData;
        }
        case BASIC_TYPE_CHAR: {
          uint8_t *value = malloc(sizeof(uint8_t));
          *value = (uint8_t) getNormalizedBoolValueFromUInt32(oldValueData->data);
          valueData->data = value;

          return valueData;
        }
        default: {
          ASSERT(false, "Type not supported!");
          break;
        }
      }
      break;
    }
    case BASIC_TYPE_INT: {
      switch (oldType.basicType) {
        case BASIC_TYPE_BOOL: {
          uint64_t *value = malloc(sizeof(uint64_t));
          *value = getBoolValue(oldValueData->data);
          valueData->data = value;

          return valueData;
        }
        case BASIC_TYPE_CHAR: {
          uint64_t *value = malloc(sizeof(uint64_t));
          *value = (uint64_t) getCharValue(oldValueData->data);
          valueData->data = value;

          return valueData;
        }
        default: {
          ASSERT(false, "Type not supported!");
          break;
        }
      }
      break;
    }
    case BASIC_TYPE_CHAR: {
      switch (oldType.basicType) {
        case BASIC_TYPE_BOOL: {
          uint32_t *value = malloc(sizeof(uint32_t));
          *value = getBoolValue(oldValueData->data);
          valueData->data = value;

          return valueData;
        }
        case BASIC_TYPE_INT: {
          uint32_t *value = malloc(sizeof(uint32_t));
          *value = getNormalizedCharValueFromUInt64(oldValueData->data);
          valueData->data = value;

          return valueData;
        }
        default: {
          ASSERT(false, "Type not supported!");
          break;
        }
      }
      break;
    }
    case BASIC_TYPE_ARRAY: {
      ArrayType *to = newType.data;
      switch (oldType.basicType) {
        case BASIC_TYPE_ARRAY: {
          ArrayType *from = oldType.data;
          if(!canTypesConvert(&to->type, &from->type)) {
            ASSERT(false, "Type not supported!");
            break;
          }
          TokenPriorityData *fromPD = oldValueData->data, *toPD = createPriorityData(fromPD->count, fromPD->parent);

          for(size_t i = 0;i < fromPD->count;i++) {
            Token *token = fromPD->instructions[i];
            if(token->type == TOKEN_NAME || isOperationTokenType(token->type)) {
              toPD->instructions[i] = token;
              continue;
            }
            if(token->type != TOKEN_VALUE) {
              ASSERT(false, "Unknown token type!");
              break;
            }
            ValueData *toValue = convertValueData(token->data, to->type);
            Token *valueToken = toPD->instructions[i] = createToken(token);
            valueToken->data = toValue;
          }

          valueData->data = toPD;

          return valueData;
        }
        default: {
          ASSERT(false, "Type not supported!");
          break;
        }
      }
      break;
    }
    default: {
      ASSERT(false, "Type not supported!");
      break;
    }
  }
  
  ASSERT(false, "Type not supported!");
  return NULL;
}

void optimizeConstVariables(Program *program, HashTable *constValues) {
  ASSERT(TOKEN_COUNT == 30, "Not all operations are implemented in optimizeConstVariables!");
  ASSERT(BASIC_TYPES_COUNT == 7, "Not all types are implemented in optimizeConstVariables!");
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
          case BASIC_TYPE_CHAR:
          case BASIC_TYPE_FUNCTION:
          case BASIC_TYPE_ARRAY: {
            setElementInHashTable(
              constValues,
              nameData->name,
              convertValueData(
                valueData,
                *nameData->type
              )
            );

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
        setElementInHashTable(
          constValues,
          nameData->name,
          convertValueData(
            getElementFromHashTable(constValues, nextNameData->name),
            *nameData->type
          )
        );

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
      *inputName->offset = offset + getTypeByteSize(inputName->type);
      offset = offset + getTypeByteOffset(inputName->type);
    }

    recalculateOffsets(data->body);
  }
}

void optimizeProgram(Program *program) {
  (void) program;
  clock_t startClock = clock(), optimizerStart = startClock;

  // optimizeConstVariables(program, NULL);
  // startClock = clock() - startClock;
  // printf("[LOG]: [OPTIMIZER] Optimizing const variables   : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  // startClock = clock();

  // cleanOffsets(program);
  // startClock = clock() - startClock;
  // printf("[LOG]: [OPTIMIZER] Cleaning offsets             : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  // startClock = clock();

  // recalculateOffsets(program);
  // startClock = clock() - startClock;
  // printf("[LOG]: [OPTIMIZER] Recalculating offsets        : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
  // startClock = clock();

  startClock = clock() - optimizerStart;
  printf("[LOG]: [OPTIMIZER] Optimizer                    : %f sec\n", ((double) startClock)/CLOCKS_PER_SEC);
}
