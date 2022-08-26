#include "../../tokenizer.h";

// Linux x86_64
#ifdef LINUX_x86_64

size_t getTypeByteSize(Type type) {
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

#endif
