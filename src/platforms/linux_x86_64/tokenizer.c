// Linux x86_64
#include "tokenizer.h"
#include "../../utils/utils.h"

size_t getTypeByteSize_linux_x86_64(Type type) {
  // We have to round up to the upper multiplier of 8
  // Because of the weird but good CPU thing called
  // Memory Alignment
  switch (type.basicType) {
    case BASIC_TYPE_INT:      return 8; // Actual size: 8
    case BASIC_TYPE_BOOL:     return 8; // Actual size: 1
    case BASIC_TYPE_FUNCTION: return 8; // Pointer size: 8
  
    default: {
      printf("type: %d %s\n", type.basicType, getTypeName(type));
      ASSERT(false, "Unreachable in `getTypeByteSize`!");
      break;
    }
  }
  return 0;
}
