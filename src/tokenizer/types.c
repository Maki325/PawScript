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
  ASSERT(BASIC_TYPES_COUNT == 5, "Not all types are implemented in areTypesEqual!");

  if(a.basicType != b.basicType) return false;

  switch (a.basicType) {
    case BASIC_TYPE_INT:
    case BASIC_TYPE_BOOL:
    case BASIC_TYPE_VOID:
    case BASIC_TYPE_NONE:
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

    default: {
      ASSERT(false, "Unreachable!");
      return false;
    }
  }
}


// Constants
Type CONST_TYPE_NONE     = {.basicType = BASIC_TYPE_NONE,     .data = NULL};
Type CONST_TYPE_INT      = {.basicType = BASIC_TYPE_INT,      .data = NULL};
Type CONST_TYPE_BOOL     = {.basicType = BASIC_TYPE_BOOL,     .data = NULL};
Type CONST_TYPE_VOID     = {.basicType = BASIC_TYPE_VOID,     .data = NULL};
Type CONST_TYPE_FUNCTION = {.basicType = BASIC_TYPE_FUNCTION, .data = NULL};
