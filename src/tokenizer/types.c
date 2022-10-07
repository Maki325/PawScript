#include "types.h"

Type *createType(BasicType basicType) {
  Type *type = malloc(sizeof(Type));
  type->basicType = basicType;
  type->data = NULL;
  return type;
}
Type *createNoneType() {
  return createType(BASIC_TYPE_NONE);
}

bool areTypesEqual(Type a, Type b) {
  ASSERT(BASIC_TYPES_COUNT == 7, "Not all types are implemented in areTypesEqual!");

  if(a.basicType != b.basicType) return false;

  switch (a.basicType) {
    case BASIC_TYPE_INT:
    case BASIC_TYPE_BOOL:
    case BASIC_TYPE_VOID:
    case BASIC_TYPE_NONE:
    case BASIC_TYPE_CHAR:
    case BASIC_TYPES_COUNT: return true;
    case BASIC_TYPE_FUNCTION: {
      if(a.data == b.data) return true;
      if(!a.data || b.data) return false;
      FunctionType *typeA = a.data, *typeB = b.data;

      if(!areTypesEqual(typeA->output, typeB->output)) return false;
      if(typeA->inputSize != typeB->inputSize) return false;
      for(size_t i = 0;i < typeA->inputSize;i++) {
        if(typeA->input[i] == typeB->input[i]) continue;
        if(!typeA->input[i] || !typeB->input[i]) return false;
        if(!areTypesEqual(*typeA->input[i], *typeB->input[i])) return false;
      }
      return true;
    }
    case BASIC_TYPE_ARRAY: {
      if(a.data == b.data) return true;
      if(!a.data || b.data) return false;
      ArrayType *typeA = a.data, *typeB = b.data;

      if(!areTypesEqual(typeA->type, typeB->type)) return false;
      if(typeA->numberOfElements != typeB->numberOfElements) return false;
      return true;
    }

    default: {
      ASSERT(false, "Unreachable!");
      return false;
    }
  }
}

bool canTypesConvert(Type a, Type b) {
  if(areTypesEqual(a, b)) return true;
  BasicType basicB = b.basicType;
  switch(a.basicType) {
    case BASIC_TYPE_INT: {
      return basicB == BASIC_TYPE_BOOL || basicB == BASIC_TYPE_CHAR;
    }
    case BASIC_TYPE_BOOL: {
      return basicB == BASIC_TYPE_INT || basicB == BASIC_TYPE_CHAR;
    }
    case BASIC_TYPE_CHAR: {
      return basicB == BASIC_TYPE_INT || basicB == BASIC_TYPE_BOOL;
    }
    case BASIC_TYPE_FUNCTION: {
      FunctionType *af = a.data, *bf = b.data;
      if(!canTypesConvert(af->output, bf->output)) return false;
      if(af->inputSize != bf->inputSize) return false;
      for(size_t i = 0;i < af->inputSize;i++) {
        if(af->input[i] == bf->input[i]) continue;
        if(!af->input[i] || !bf->input[i]) return false;
        if(!canTypesConvert(*af->input[i], *bf->input[i])) return false;
      }

      return false;
    }
    case BASIC_TYPE_ARRAY: {
      ArrayType *aa = a.data, *ba = b.data;
      if(aa->numberOfElements != ba->numberOfElements) return false;
      if(!canTypesConvert(aa->type, ba->type)) return false;

      return true;
    }
    default: {
      ASSERT(false, "Unreachable!");
    }
  }
}

// Constants
Type CONST_TYPE_NONE     = {.basicType = BASIC_TYPE_NONE,     .data = NULL};
Type CONST_TYPE_INT      = {.basicType = BASIC_TYPE_INT,      .data = NULL};
Type CONST_TYPE_BOOL     = {.basicType = BASIC_TYPE_BOOL,     .data = NULL};
Type CONST_TYPE_VOID     = {.basicType = BASIC_TYPE_VOID,     .data = NULL};
Type CONST_TYPE_FUNCTION = {.basicType = BASIC_TYPE_FUNCTION, .data = NULL};
