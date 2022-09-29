#ifndef TYPES_H_
#define TYPES_H_
#include "../includes.h"

typedef enum BasicType {
  BASIC_TYPE_NONE = 0,
  BASIC_TYPE_INT,
  BASIC_TYPE_BOOL,
  BASIC_TYPE_VOID,
  BASIC_TYPE_FUNCTION,
  BASIC_TYPES_COUNT
} BasicType;

typedef struct Type {
  BasicType basicType;
  void *data;
} Type;

typedef struct FunctionType {
  Type **input;
  size_t inputSize;
  Type output;
} FunctionType;

Type *createType(BasicType basicType);
Type *createNoneType();

bool areTypesEqual(Type a, Type b);

// Constants
extern Type CONST_TYPE_NONE;
extern Type CONST_TYPE_INT;
extern Type CONST_TYPE_BOOL;
extern Type CONST_TYPE_VOID;
extern Type CONST_TYPE_FUNCTION;

#endif // TYPES_H_