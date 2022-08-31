// Linux x86_64
#include "tokenizer.h"

size_t getTypeByteSize_linux_x86_64(Type type) {
  // We have to round up to the upper multiplier of 8
  // Because of the weird but good CPU thing called
  // Memory Alignment
  switch (type) {
    case TYPE_INT:  return 8; // Actual size: 8
    case TYPE_BOOL: return 8; // Actual size: 1
  
    default: {
      ASSERT(true, "Unreachable in `getTypeByteSize`!");
      break;
    }
  }
  return 0;
}
