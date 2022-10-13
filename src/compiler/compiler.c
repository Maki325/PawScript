#include "compiler.h"
#include "../utils/utils.h"

const char *get64BitRegister(Register reg) {
  ASSERT(REGISTER_COUNT == 16, "Not all registers are implemented in get64BitRegister!");
  switch(reg) {
    case REGISTER_A:      return "rax";
    case REGISTER_B:      return "rbx";
    case REGISTER_C:      return "rcx";
    case REGISTER_D:      return "rdx";
    case REGISTER_SI:     return "rsi";
    case REGISTER_DI:     return "rdi";
    case REGISTER_BP:     return "rbp";
    case REGISTER_SP:     return "rsp";
    case REGISTER_8:      return "r8";
    case REGISTER_9:      return "r9";
    case REGISTER_10:     return "r10";
    case REGISTER_11:     return "r11";
    case REGISTER_12:     return "r12";
    case REGISTER_13:     return "r13";
    case REGISTER_14:     return "r14";
    case REGISTER_15:     return "r15";
    case REGISTER_COUNT:  ASSERT(false, "REGISTER ERROR!");
  }
  return "REGISTER ERROR!";
}
const char *get32BitRegister(Register reg) {
  ASSERT(REGISTER_COUNT == 16, "Not all registers are implemented in get32BitRegister!");
  switch(reg) {
    case REGISTER_A:      return "eax";
    case REGISTER_B:      return "ebx";
    case REGISTER_C:      return "ecx";
    case REGISTER_D:      return "edx";
    case REGISTER_SI:     return "esi";
    case REGISTER_DI:     return "edi";
    case REGISTER_BP:     return "ebp";
    case REGISTER_SP:     return "esp";
    case REGISTER_8:      return "r8d";
    case REGISTER_9:      return "r9d";
    case REGISTER_10:     return "r10d";
    case REGISTER_11:     return "r11d";
    case REGISTER_12:     return "r12d";
    case REGISTER_13:     return "r13d";
    case REGISTER_14:     return "r14d";
    case REGISTER_15:     return "r15d";
    case REGISTER_COUNT:  ASSERT(false, "REGISTER ERROR!");
  }
  return "REGISTER ERROR!";
}
const char *get16BitRegister(Register reg) {
  ASSERT(REGISTER_COUNT == 16, "Not all registers are implemented in get16BitRegister!");
  switch(reg) {
    case REGISTER_A:      return "ax";
    case REGISTER_B:      return "bx";
    case REGISTER_C:      return "cx";
    case REGISTER_D:      return "dx";
    case REGISTER_DI:     return "di";
    case REGISTER_SI:     return "si";
    case REGISTER_BP:     return "bp";
    case REGISTER_SP:     return "sp";
    case REGISTER_8:      return "r8w";
    case REGISTER_9:      return "r9w";
    case REGISTER_10:     return "r10w";
    case REGISTER_11:     return "r11w";
    case REGISTER_12:     return "r12w";
    case REGISTER_13:     return "r13w";
    case REGISTER_14:     return "r14w";
    case REGISTER_15:     return "r15w";
    case REGISTER_COUNT:  ASSERT(false, "REGISTER ERROR!");
  }
  return "REGISTER ERROR!";
}
const char *get8BitRegister(Register reg) {
  ASSERT(REGISTER_COUNT == 16, "Not all registers are implemented in get8BitRegister!");
  switch(reg) {
    case REGISTER_A:      return "al";
    case REGISTER_B:      return "bl";
    case REGISTER_C:      return "cl";
    case REGISTER_D:      return "dl";
    case REGISTER_SI:     return "sil";
    case REGISTER_DI:     return "dil";
    case REGISTER_BP:     return "bpl";
    case REGISTER_SP:     return "spl";
    case REGISTER_8:      return "r8b";
    case REGISTER_9:      return "r9b";
    case REGISTER_10:     return "r10b";
    case REGISTER_11:     return "r11b";
    case REGISTER_12:     return "r12b";
    case REGISTER_13:     return "r13b";
    case REGISTER_14:     return "r14b";
    case REGISTER_15:     return "r15b";
    case REGISTER_COUNT:  ASSERT(false, "REGISTER ERROR!");
  }
  return "REGISTER ERROR!";
}

const char *getRegisterBySize(Register reg, Type type) {
  switch(getTypeByteSize(type)) {
    case 1: return get8BitRegister(reg);
    case 2: return get16BitRegister(reg);
    case 4: return get32BitRegister(reg);
    case 8: return get64BitRegister(reg);
    default: ASSERT(false, "Unknown Type!");
  }
}

char *getInitializedType(Type type) {
  ASSERT(BASIC_TYPES_COUNT == 5, "Not all types are implemented in getInitializedType!");
  static char* bytes[BASIC_TYPES_COUNT + 1] = {
    /* NONE     */ "ERROR NONE!!!",
    /* INT      */ "DQ",
    /* BOOL     */ "DQ",
    /* VOID     */ "ERROR VOID!!!",
    /* FUNCTION */ "ERROR FUNCTION!!!",
    /* COUNT    */ "ERROR COUNT!!!",
  };
  if(type.basicType > BASIC_TYPES_COUNT) return "ERROR UNKNOWN!!!";
  return bytes[type.basicType];
}
char *getUninitializedType(Type type) {
  ASSERT(BASIC_TYPES_COUNT == 5, "Not all types are implemented in getUninitializedType!");
  static char* bytes[BASIC_TYPES_COUNT + 1] = {
    /* NONE     */ "ERROR NONE!!!",
    /* INT      */ "RESQ",
    /* BOOL     */ "RESQ",
    /* VOID     */ "ERROR VOID!!!",
    /* FUNCTION */ "ERROR FUNCTION!!!",
    /* COUNT    */ "ERROR COUNT!!!",
  };
  if(type.basicType > BASIC_TYPES_COUNT) return "ERROR UNKNOWN!!!";
  return bytes[type.basicType];
}

void addPrintFunction(FILE *out) {
  fputs("print64:\n", out);
  fputs("  mov   r9, -3689348814741910323\n", out);
  fputs("  sub   rsp, 40\n", out);
  fputs("  mov   BYTE [rsp+31], 10\n", out);
  fputs("  lea   rcx, [rsp+30]\n", out);
  fputs(".print64L2:\n", out);
  fputs("  mov   rax, rdi\n", out);
  fputs("  lea   r8, [rsp+32]\n", out);
  fputs("  mul   r9\n", out);
  fputs("  mov   rax, rdi\n", out);
  fputs("  sub   r8, rcx\n", out);
  fputs("  shr   rdx, 3\n", out);
  fputs("  lea   rsi, [rdx+rdx*4]\n", out);
  fputs("  add   rsi, rsi\n", out);
  fputs("  sub   rax, rsi\n", out);
  fputs("  add   eax, 48\n", out);
  fputs("  mov   BYTE [rcx], al\n", out);
  fputs("  mov   rax, rdi\n", out);
  fputs("  mov   rdi, rdx\n", out);
  fputs("  mov   rdx, rcx\n", out);
  fputs("  sub   rcx, 1\n", out);
  fputs("  cmp   rax, 9\n", out);
  fputs("  ja  .print64L2\n", out);
  fputs("  lea   rax, [rsp+32]\n", out);
  fputs("  mov   edi, 1\n", out);
  fputs("  sub   rdx, rax\n", out);
  fputs("  xor   eax, eax\n", out);
  fputs("  lea   rsi, [rsp+32+rdx]\n", out);
  fputs("  mov   rdx, r8\n", out);
  fputs("  mov   rax, 1\n", out);
  fputs("  syscall\n", out);
  fputs("  add   rsp, 40\n", out);
  fputs("ret\n", out);
}
void addCharPrintFunction(FILE *out) {
  fputs("; edi - input 32 bit char\n", out);
  fputs("printChar:\n", out);
  fputs("  ; JA SAM KORISTIO\n", out);
  fputs("  ; RSP\n", out);
  fputs("  ; UMESTO JEBENI\n", out);
  fputs("  ; RBP\n", out);
  fputs("  ; 😡\n", out);
  fputs("\n", out);
  fputs("  push rbp\n", out);
  fputs("  mov rbp, rsp\n", out);
  fputs("  sub rsp, 8\n", out);
  fputs("  mov rdx, 0\n", out);
  fputs("\n", out);
  fputs("  mov eax, edi\n", out);
  fputs("  mov DWORD [rbp - 4], 0\n", out);
  fputs("\n", out);
  fputs("  ; So we can more easly use each byte\n", out);
  fputs("  mov [rbp - 8], eax\n", out);
  fputs("\n", out);
  fputs("  cmp edi, 0x7F\n", out);
  fputs("  jg printChar2\n", out);
  fputs("  mov [rbp - 4], eax\n", out);
  fputs("  mov rdx, 1\n", out);
  fputs("  jmp printCharPrint\n", out);
  fputs("\n", out);
  fputs("  printChar2:\n", out);
  fputs("  cmp edi, 0x7FF\n", out);
  fputs("  jg printChar3\n", out);
  fputs("\n", out);
  fputs("  mov bx, [rbp - 8]\n", out);
  fputs("  shr bx, 6\n", out);
  fputs("  or bx, 0xC0\n", out);
  fputs("  mov [rbp - 4], bl\n", out);
  fputs("\n", out);
  fputs("  mov bx, [rbp - 8]\n", out);
  fputs("  and bx, 0x3F\n", out);
  fputs("  or bx, 0x80\n", out);
  fputs("  mov [rbp - 3], bl\n", out);
  fputs("\n", out);
  fputs("  mov rdx, 2\n", out);
  fputs("  jmp printCharPrint\n", out);
  fputs("\n", out);
  fputs("  printChar3:\n", out);
  fputs("  cmp edi, 0xFFFF\n", out);
  fputs("  jg printChar4\n", out);
  fputs("\n", out);
  fputs("  mov bx, [rbp - 8]\n", out);
  fputs("  shr bx, 12\n", out);
  fputs("  or bx, 0xE0\n", out);
  fputs("  mov [rbp - 4], bl\n", out);
  fputs("\n", out);
  fputs("  mov bx, [rbp - 8]\n", out);
  fputs("  shr bx, 6\n", out);
  fputs("  and bx, 0x3F\n", out);
  fputs("  or bx, 0x80\n", out);
  fputs("  mov [rbp - 3], bl\n", out);
  fputs("\n", out);
  fputs("  mov bx, [rbp - 8]\n", out);
  fputs("  and bx, 0x3F\n", out);
  fputs("  or bx, 0x80\n", out);
  fputs("  mov [rbp - 2], bl\n", out);
  fputs("\n", out);
  fputs("  mov rdx, 3\n", out);
  fputs("  jmp printCharPrint\n", out);
  fputs("\n", out);
  fputs("  printChar4:\n", out);
  fputs("  cmp edi, 0x10FFFF\n", out);
  fputs("  jg printCharEnd ; Any larger then this is invalid, I think, and I hope\n", out);
  fputs("\n", out);
  fputs("  mov ebx, [rbp - 8]\n", out);
  fputs("  shr ebx, 18\n", out);
  fputs("  or bx, 0xF0\n", out);
  fputs("  mov [rbp - 4], bl\n", out);
  fputs("\n", out);
  fputs("  mov ebx, [rbp - 8]\n", out);
  fputs("  shr ebx, 12\n", out);
  fputs("  and bx, 0x3F\n", out);
  fputs("  or bx, 0x80\n", out);
  fputs("  mov [rbp - 3], bl\n", out);
  fputs("\n", out);
  fputs("  mov ebx, [rbp - 8]\n", out);
  fputs("  shr ebx, 6\n", out);
  fputs("  and bx, 0x3F\n", out);
  fputs("  or bx, 0x80\n", out);
  fputs("  mov [rbp - 2], bl\n", out);
  fputs("\n", out);
  fputs("  mov ebx, [rbp - 8]\n", out);
  fputs("  and ebx, 0x3F\n", out);
  fputs("  or ebx, 0x80\n", out);
  fputs("  mov [rbp - 1], bl\n", out);
  fputs("\n", out);
  fputs("  mov rdx, 4\n", out);
  fputs("  jmp printCharPrint\n", out);
  fputs("\n", out);
  fputs("  printCharPrint:\n", out);
  fputs("\n", out);
  fputs("  mov rax, 1\n", out);
  fputs("  mov rdi, 1\n", out);
  fputs("  lea rsi, [rbp - 4]\n", out);
  fputs("  ; RDX is length\n", out);
  fputs("  syscall\n", out);
  fputs("\n", out);
  fputs("  printCharEnd:\n", out);
  fputs("  mov rsp, rbp\n", out);
  fputs("  pop rbp\n", out);
  fputs("ret\n", out);
}
void prepareFileForCompile(FILE *out) {
  fputs("BITS 64\n", out);
  fputs("section .text\n", out);
  addPrintFunction(out);
  addCharPrintFunction(out);
}
void postCompile(CompilerOptions *compilerOptions) {
  fputs("global _start\n", compilerOptions->output);
  fputs("_start:\n", compilerOptions->output);

  char *mainFunctioName = NULL;
  HashTable *functions = compilerOptions->program->functions;
  for(size_t i = 0;i < functions->capacity;i++) {
    if(functions->elements[i].key == NULL) {
      continue;
    }
    FunctionDefinition *data = functions->elements[i].value;
    if(data->isMain) {
      if(mainFunctioName) {
        exitError(ERROR_MULTIPLE_MAIN_FUNCTIONS);
      }
      mainFunctioName = (char*) data->name;
    }
  }
  if(!mainFunctioName) {
    exitError(ERROR_NO_MAIN_FUNCTION);
  }

  fprintf(compilerOptions->output, "call %s\n", mainFunctioName);
  // RDI is the return code of the program
  // Which is returned from the main function in the RAX register
  fputs("mov rdi, rax\n", compilerOptions->output);
  // RAX is the syscall code for exiting the program
  fputs("mov rax, 60\n", compilerOptions->output);
  fputs("syscall\n", compilerOptions->output);
}

void generateFunctionAsm(CompilerOptions *compilerOptions, FunctionDefinition *functionData) {
  // Optimize to use register for some fields instead of stack because faster 👍
  fprintf(
    compilerOptions->output,
    "; --- FUNCTION %s ---\n",
    functionData->variableName
  );
  fprintf(compilerOptions->output, "%s:\n", functionData->name);
  fputs("push rbp\n", compilerOptions->output);
  fputs("mov rbp, rsp\n", compilerOptions->output);
  fprintf(compilerOptions->output, "sub rsp, %" PRIi32 "\n", -functionData->body->variableOffset);

  generateProgramAsm(compilerOptions, functionData->body);

  fputs("mov rsp, rbp\n", compilerOptions->output);
  fputs("pop rbp\n", compilerOptions->output);
  fputs("ret\n", compilerOptions->output);
}

Token *nextToken(Program *program, size_t *i) {
  Token *token = program->instructions[++(*i)];
  return token;
}

void generateBinaryOperationAsm(CompilerOptions *compilerOptions, Program *program, Token *operationToken) {
  BinaryOperationData *data = operationToken->data;
  Token *left = data->operandOne, *right = data->operandTwo;
  fprintf(
    compilerOptions->output,
    "; --- BINARY OPERATION %s | TYPE %s ---\n",
    getTokenTypeName(operationToken->type),
    getTypeName(data->type)
  );

  switch (left->type) {
    case TOKEN_VALUE: {
      ValueData *valueData = left->data;
      switch(valueData->type.basicType) {
        case BASIC_TYPE_INT: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION LEFT VALUE INT %" PRIu64 " ---\n",
            getIntValue(valueData->data)
          );
          fprintf(
            compilerOptions->output,
            "mov rax, %" PRIu64 "\n",
            getIntValue(valueData->data)
          );
          break;
        }
        case BASIC_TYPE_BOOL: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION LEFT VALUE BOOL %" PRIu8 " ---\n",
            getBoolValue(valueData->data)
          );

          fprintf(
            compilerOptions->output,
            "mov eax, %" PRIu8 "\n",
            getBoolValue(valueData->data)
          );
          break;
        }
        default: {
          ASSERT(false, "Type not supported in `generateBinaryOperationAsm`!");
        }
      }
      break;
      break;
    }
    case TOKEN_NAME: {
      generateNameAsm(compilerOptions, program, left, REGISTER_A);
      break;
    }
    default: {
      if(!isOperationTokenType(left->type)) {
        if(left->type != TOKEN_PRIORITY) {
          ASSERT(false, "Type not supported!");
        }
        TokenPriorityData *data = left->data;
        if(data->count != 1) {
          ASSERT(false, "Type not supported!");
        }
        left = data->instructions[0];
      }
      if(!isOperationTokenType(left->type)) {
        ASSERT(false, "Type not supported!");
      }
      fprintf(
        compilerOptions->output,
        "; --- BINARY OPERATION LEFT | BINARY OPERATION ---\n"
      );
      generateBinaryOperationAsm(compilerOptions, program, left);
      break;
    }
  }

  switch (right->type) {
    case TOKEN_VALUE: {
      ValueData *valueData = right->data;
      switch(valueData->type.basicType) {
        case BASIC_TYPE_INT: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION RIGHT VALUE INT %" PRIu64 " ---\n",
            getIntValue(valueData->data)
          );
          fprintf(
            compilerOptions->output,
            "mov rbx, %" PRIu64 "\n",
            getIntValue(valueData->data)
          );
          break;
        }
        case BASIC_TYPE_BOOL: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION RIGHT VALUE BOOL %" PRIu8 " ---\n",
            getBoolValue(valueData->data)
          );

          fprintf(
            compilerOptions->output,
            "mov ebx, %" PRIu8 "\n",
            getBoolValue(valueData->data)
          );
          break;
        }
        default: {
          ASSERT(false, "Type not supported in `generateBinaryOperationAsm`!");
        }
      }
      break;
      break;
    }
    case TOKEN_NAME: {
      generateNameAsm(compilerOptions, program, right, REGISTER_B);
      break;
    }
    default: {
      if(!isOperationTokenType(right->type)) {
        if(right->type != TOKEN_PRIORITY) {
          ASSERT(false, "Type not supported!");
        }
        TokenPriorityData *data = right->data;
        if(data->count != 1) {
          ASSERT(false, "Type not supported!");
        }
        right = data->instructions[0];
      }
      if(!isOperationTokenType(right->type)) {
        ASSERT(false, "Type not supported!");
      }

      fprintf(
        compilerOptions->output,
        "; --- BINARY OPERATION RIGHT | BINARY OPERATION ---\n"
      );
      fputs("push rax\n", compilerOptions->output);

      Program p = {
        .parent = program->parent,
        .capacity = program->capacity,
        .count = program->count,
        .variables = program->variables,
        .variableOffset = program->variableOffset + 8,
        .functions = program->functions,
        .id = program->id,
        .instructions = program->instructions,
        .useInOffsetCalculations = true,
      };
      generateBinaryOperationAsm(compilerOptions, &p, right);
      fputs("mov rbx, rax\n", compilerOptions->output);
      fputs("pop rax\n", compilerOptions->output);
      break;
    }
  }

  // TODO: [IMPORTANT]: Do operations based on byte size (i.e. only ax, or eax, or rax)
  switch (operationToken->type) {
    case TOKEN_ADD: {
      fputs("add rax, rbx\n", compilerOptions->output);
      break;
    }
    case TOKEN_SUBTRACT: {
      fputs("sub rax, rbx\n", compilerOptions->output);
      break;
    }
    case TOKEN_GREATER_THAN:
    case TOKEN_LESS_THAN:
    case TOKEN_EQUALS:
    case TOKEN_NOT_EQUALS: {
      fputs("push rcx\n", compilerOptions->output);
      fputs("push rdx\n", compilerOptions->output);

      // Prepare compare values
      fputs("mov ecx, 0\n", compilerOptions->output);
      fputs("mov edx, 1\n", compilerOptions->output);

      fputs("cmp rax, rbx\n", compilerOptions->output);

      // Agains, using eYx, not rYx, because speed
      if(operationToken->type == TOKEN_GREATER_THAN) {
        fputs("cmovg ecx, edx\n", compilerOptions->output);
      } else if(operationToken->type == TOKEN_LESS_THAN) {
        fputs("cmovl ecx, edx\n", compilerOptions->output);
      } else if(operationToken->type == TOKEN_EQUALS) {
        fputs("cmove ecx, edx\n", compilerOptions->output);
      } else if(operationToken->type == TOKEN_NOT_EQUALS) {
        fputs("cmovne ecx, edx\n", compilerOptions->output);
      }

      fputs("mov eax, ecx\n", compilerOptions->output);

      fputs("pop rdx\n", compilerOptions->output);
      fputs("pop rcx\n", compilerOptions->output);
      break;
    }
    default: {
      ASSERT(false, "Type not supported!");
      break;
    }
  }

  fprintf(
    compilerOptions->output,
    "; --- BINARY OPERATION %s | TYPE %s | END ---\n",
    getTokenTypeName(operationToken->type),
    getTypeName(data->type)
  );
}

int32_t calculateOffset(Program *program, NameData *nameData) {
  if(!program->variables) {
    if(program->parent) {
      if(!program->useInOffsetCalculations) {
        return calculateOffset(program->parent, nameData);
      }
      return abs(program->parent->variableOffset) + 8 + calculateOffset(program->parent, nameData);
    }
    ASSERT(false, "Can't find variable offset!");
  }
  if(existsElementInHashTable(program->variableMap, nameData->name)) {
    return *nameData->offset;
  }
  
  if(!program->useInOffsetCalculations) {
    return calculateOffset(program->parent, nameData);
  }
  return abs(program->parent->variableOffset) + 8 + calculateOffset(program->parent, nameData);
}

void generateFunctionCallNameAsm(CompilerOptions *compilerOptions, Program *program, Token *token, size_t offsetTo) {
  NameData *nameData = token->data;

  (void) nameData;
  (void) offsetTo;

  NameData *data = token->data;
  int32_t offsetFrom = calculateOffset(program, data);
  const char *sign = getSign(offsetFrom);
  int32_t offsetValue = abs(offsetFrom);

  fprintf(
    compilerOptions->output,
    "; --- FUNCTION CALL TOKEN NAME %s ---\n",
    data->variableName
  );
  switch(data->type->basicType) {
    case BASIC_TYPE_INT: {
      fprintf(
        compilerOptions->output,
        "mov rax, [rbp %s %" PRIi32 "]\n",
        sign,
        offsetValue
      );
      fprintf(
        compilerOptions->output,
        "mov [rbp %s %" PRIi32 "], rax\n",
        sign,
        offsetValue
      );
      break;
    }
    case BASIC_TYPE_BOOL: {
      fprintf(
        compilerOptions->output,
        "movzx eax, BYTE [rbp %s %" PRIi32 "]\n",
        sign,
        offsetValue
      );
      break;
    }
    case BASIC_TYPE_CHAR: {
      fprintf(
        compilerOptions->output,
        "mov eax, [rbp %s %" PRIi32 "]\n",
        sign,
        offsetValue
      );
      break;
    }
    case BASIC_TYPE_FUNCTION: {
      fprintf(
        compilerOptions->output,
        "mov rax, [rbp %s %" PRIi32 "]\n",
        sign,
        offsetValue
      );
      break;
    }
    default: {
      ASSERT(false, "Type not supported!");
    }
  }
}

Type **getParameterTypes(FunctionCallData *data) {
  if(data->nameData) {
    FunctionType *functionType = data->nameData->type->data;
    return functionType->input;
  } else if(data->function) {
    return data->function->functionType->input;
  } else {
    ASSERT(false, "Unreachable!");
  }
}

void generateFunctionCallAsm(CompilerOptions *compilerOptions, Program *program, Token *token) {
  FunctionCallData *data = token->data;
  NameData *nameData = data->nameData;
  TokenPriorityData *arguments = data->arguments;
  Type **parameterTypes = getParameterTypes(data);

  fprintf(
    compilerOptions->output,
    "; --- FUNCTION CALL %s ---\n",
    getFunctionNameFromCall(data)
  );

  size_t bytes = 0;
  for(size_t i = 0;i < arguments->count;i++) {
    Token *arg = arguments->instructions[i];
    if(arg->type == TOKEN_NAME) {
      NameData *nameData = arg->data;
      bytes += getTypeByteOffset(*nameData->type);
    } else if(arg->type == TOKEN_INDEX) {
      IndexData *indexData = arg->data;
      NameData *nameData = indexData->nameData;
      ArrayType *arrayType = nameData->type->data;

      bytes += getTypeByteOffset(arrayType->type);
    } else if(arg->type == TOKEN_VALUE) {
      ValueData *valueData = arg->data;
      bytes += getTypeByteOffset(valueData->type);
    } else if(isOperationTokenType(arg->type)) {
      BinaryOperationData *data = arg->data;
      bytes += getTypeByteOffset(data->type);
    } else {
      printf("arg->type: %d %s\n", arg->type, getTokenTypeName(arg->type));
      ASSERT(false, "Type not supported!");
    }
  }
  
  size_t byteSize = bytes;
  if(byteSize != 0) {
    fputs("push rbp\n", compilerOptions->output);
    fputs("mov rbp, rsp\n", compilerOptions->output);
    fprintf(compilerOptions->output, "sub rsp, %zu\n", byteSize);
    Program p = {
      .parent = program,
      .capacity = 0,
      .count = 0,
      .variables = NULL,
      .variableOffset = byteSize,
      .functions = NULL,
      .id = 0,
      .instructions = NULL,
      .useInOffsetCalculations = true,
    };

    bytes = 8;
    for(size_t i = 0;i < arguments->count;i++) {
      Token *arg = arguments->instructions[i];

      if(arg->type == TOKEN_NAME) {
        NameData *nameData = arg->data;
        size_t parameterSize = getTypeByteSize(*parameterTypes[i]);
        generateAddressAssignAsm(
          compilerOptions,
          &p,
          arg,
          *parameterTypes[i],
          parameterSize <= 8 ? -(bytes - parameterSize) : -(bytes - 8 + parameterSize),
          *nameData->offset < 0
        );

        bytes += getTypeByteOffset(*nameData->type);
      } else if(arg->type == TOKEN_INDEX) {
        IndexData *indexData = arg->data;
        NameData *nextData = indexData->nameData;
        ArrayType *arrayType = nextData->type->data;

        generateIndexAsm(
          compilerOptions,
          &p,
          arg,
          REGISTER_A
        );
        generateIntoNameAssignAsm(
          compilerOptions,
          -(bytes - getTypeByteSize(arrayType->type)),
          *parameterTypes[i],
          REGISTER_A
        );

        // generateNameAssignAsm((GenerateNameAssignAsmInfo) {
        //   .compilerOptions = compilerOptions,
        //   .nextData = nextData,
        //   .nextType = arrayType->type,
        //   .nextOffset = calculateOffset(&p, nextData),
        //   .offset = -(bytes - getTypeByteSize(arrayType->type)), // I feel like this should be more calculated? Because what if it's a 32 bit?
        //   .program = &p,
        //   .type = *parameterTypes[i],
        //   .variableName = nextData->variableName,
        // });

        bytes += getTypeByteOffset(*nextData->type);
      } else if(arg->type == TOKEN_VALUE) {
        ValueData *valueData = arg->data;
        generateValueAsm(compilerOptions, arg, REGISTER_A);
        fprintf(compilerOptions->output, "mov [rbp - %zu], rax\n", bytes);

        bytes += getTypeByteOffset(valueData->type);
      } else if(isOperationTokenType(arg->type)) {
        generateBinaryOperationAsm(compilerOptions, &p, arg);
        fprintf(compilerOptions->output, "mov [rbp - %zu], rax\n", bytes);

        BinaryOperationData *data = arg->data;
        bytes += getTypeByteOffset(data->type);
      } else {
        ASSERT(false, "Type not supported!");
      }
    }
  }

  if(data->function) {
    fprintf(compilerOptions->output, "call %s\n", data->function->name);
  } else {
    Program p = {
      .parent = program,
      .capacity = 0,
      .count = 0,
      .variables = NULL,
      .variableOffset = byteSize,
      .functions = NULL,
      .id = 0,
      .instructions = NULL,
      .useInOffsetCalculations = true,
    };
    int32_t offset = calculateOffset(byteSize == 0 ? program : &p, nameData);
    fprintf(
      compilerOptions->output,
      "mov rax, [rbp %s %" PRIi32 "]\n",
      getSign(offset),
      abs(offset)
    );
    fputs("call rax\n", compilerOptions->output);
  }

  if(bytes != 0) {
    fputs("mov rsp, rbp\n", compilerOptions->output);
    fputs("pop rbp\n", compilerOptions->output);
  }
}

Type getVariableType(Token *token) {
  switch (token->type) {
    case TOKEN_NAME: {
      NameData *data = token->data;
      return *data->type;
    }
    case TOKEN_INDEX: {
      IndexData *data = token->data;
      ArrayType *type = data->nameData->type->data;
      return type->type;
    }
    case TOKEN_VALUE: {
      ValueData *data = token->data;
      return data->type;
    }
    
    default: {
      ASSERT(false, "Type not supported!");
      break;
    }
  }
}

void generateIntoNameAssignAsm(CompilerOptions *compilerOptions, int32_t offset, Type type, Register source) {
  const char *sign = getSign(offset);
  int32_t offsetValue = abs(offset);

  if(isBasicType(type)) {
    fprintf(
      compilerOptions->output,
      "mov [rbp %s %" PRIi32 "], %s\n",
      sign,
      offsetValue,
      getRegisterBySize(source, type)
    );
  } else {
    ASSERT(false, "Not implemented yet!");
  }
}

void generateAddressArrayAssignAsm(
  CompilerOptions *compilerOptions,
  Program *program,
  Token *from,
  Type toType,
  int32_t toOffset,
  bool reverse
) {
  Type fromType = getVariableType(from);
  ArrayType *fromArray = fromType.data;
  ArrayType *toArray = toType.data;

  if(from->type == TOKEN_NAME) {
    NameData *nameData = from->data;
    if(!isBasicType(fromArray->type)) {
      ASSERT(false, "Not implemented yet!");
    }
    size_t fromTypeOffset = getTypeByteOffset(fromType);
    size_t toTypeOffset = getTypeByteOffset(toType);

    size_t fromTypeElementOffset = getTypeByteOffset(fromArray->type);
    size_t toTypeElementOffset = getTypeByteOffset(toArray->type);

    size_t fromTypeElementSize = getTypeByteSize(fromArray->type);
    size_t toTypeElementSize = getTypeByteSize(toArray->type);

    int32_t offsetFrom = calculateOffset(program, nameData), fromCheck = offsetFrom;
    int32_t offsetTo = toOffset;

    // offsetTo = offsetTo < 0 ? offsetTo + toTypeOffset : offsetTo - toTypeOffset;
    // if(reverse) {
    //   offsetTo = offsetTo;
    // } else {
    //   offsetTo = offsetTo + toTypeOffset;
    // }
    offsetTo = offsetTo + toTypeOffset;
    if(reverse) {
      offsetFrom = offsetFrom;
    } else {
      offsetFrom = offsetFrom + fromTypeOffset;
    }

    for(size_t i = 0;i < toArray->numberOfElements;i++) {
      int32_t currentFromOffset = offsetFrom;
      if(reverse) {
        currentFromOffset += fromTypeElementSize;
      } else {
        currentFromOffset -= fromTypeElementSize;
      }
      const char *sign = getSign(currentFromOffset);
      int32_t offsetValue = abs(currentFromOffset);

      fprintf(
        compilerOptions->output,
        "mov %s, [rbp %s %" PRIi32 "]\n",
        getRegisterBySize(REGISTER_A, fromArray->type),
        sign,
        offsetValue
      );

      generateIntoNameAssignAsm(
        compilerOptions,
        toOffset < 0 ? offsetTo - toTypeElementSize : offsetTo + toTypeElementSize,
        toArray->type,
        REGISTER_A
      );
      
      // offsetTo = toOffset < 0 ? offsetTo - toTypeElementOffset : offsetTo + toTypeElementOffset;
      
      // if(reverse) {
      //   offsetTo = offsetTo + toTypeElementOffset;
      // } else {
      //   offsetTo = offsetTo - toTypeElementOffset;
      // }
      offsetTo = offsetTo - toTypeElementOffset;
      // offsetFrom = offsetFrom - fromTypeElementOffset;
      if(reverse) {
        offsetFrom = offsetFrom + fromTypeElementOffset;
      } else {
        offsetFrom = offsetFrom - fromTypeElementOffset;
      }
    }
    ASSERT(offsetTo == toOffset, "Offset check failed");
    if(reverse) {
      ASSERT(offsetFrom == (int32_t) (fromCheck + fromTypeElementOffset * fromArray->numberOfElements), "Offset check failed");
    } else {
      ASSERT(offsetFrom == fromCheck , "Offset check failed");
    }
  } else if(from->type == TOKEN_VALUE) {
    ValueData *valueData = from->data;
    List *list = valueData->data;

    Type nameElementType = toArray->type;

    int32_t offset = toOffset;
    size_t valueOffset = getTypeByteOffset(valueData->type);
    size_t elementSize = getTypeByteSize(nameElementType);
    size_t elementOffset = getTypeByteOffset(nameElementType);
    offset = offset < 0 ? offset + valueOffset : offset - valueOffset;
    for(size_t i = 0;i < list->size;i++) {
      Token *token = list->elements[i];

      generateValueAsm(compilerOptions, token, REGISTER_A);
      generateIntoNameAssignAsm(
        compilerOptions,
        toOffset < 0 ? offset - elementSize : toOffset + elementSize,
        nameElementType,
        REGISTER_A
      );
      offset = toOffset < 0 ? offset - elementOffset : toOffset + elementOffset;
    }
    ASSERT(offset == toOffset, "Offset check failed");
  } else {
    ASSERT(false, "Not implemented yet!");
  }
}

void generateAddressAssignAsm(
  CompilerOptions *compilerOptions,
  Program *program,
  Token *from,
  Type toType,
  int32_t toOffset,
  bool reverse
) {
  Type fromType = getVariableType(from);
  fprintf(
    compilerOptions->output,
    "; --- ASSIGN ---\n"
  );

  if(isBasicType(fromType)) {
    if(from->type == TOKEN_NAME) {
      generateNameAsm(compilerOptions, program, from, REGISTER_A);
    } else if(from->type == TOKEN_INDEX) {
      generateIndexAsm(compilerOptions, program, from, REGISTER_A);
    } else if(from->type == TOKEN_VALUE) {
      generateValueAsm(compilerOptions, from, REGISTER_A);
    } else {
      ASSERT(false, "Token not supported!");
    }

    generateIntoNameAssignAsm(
      compilerOptions,
      toOffset,
      toType,
      REGISTER_A
    );
  } else if(fromType.basicType == BASIC_TYPE_ARRAY) {
    generateAddressArrayAssignAsm(compilerOptions, program, from, toType, toOffset, reverse);
  } else {
    ASSERT(false, "Unreachable!");
  }

}

void generateVariableAssignAsm(CompilerOptions *compilerOptions, Program *program, Token *from, Token *to) {
  Type fromType = getVariableType(from);
  Type toType = getVariableType(to);

  if(!canTypesConvert(fromType, toType)) {
    exitTokenError(ERROR_TYPES_DONT_MATCH, to);
  }

  if(to->type == TOKEN_NAME) {
    NameData *data = to->data;
    generateAddressAssignAsm(
      compilerOptions,
      program,
      from,
      *data->type,
      calculateOffset(program, data),
      false
    );
  } else if(to->type == TOKEN_INDEX) {
    printToken(to, 0, 0);
    ASSERT(false, "Not implemented yet!");
  } else {
    ASSERT(false, "Token not supported!");
  }

}

// void generateNameAssignAsm(GenerateNameAssignAsmInfo info) {
//   // Program *program = info.program;
//   CompilerOptions *compilerOptions = info.compilerOptions;
//   const char *variableName = info.variableName;
//   Type type = info.type;
//   BasicType basicType = type.basicType;
//
//   int32_t offset = info.offset;
//   const char *offsetSign = getSign(offset);
//   int32_t offsetValue = abs(offset);
//
//   NameData *nextData = info.nextData;
//   Type nextType = info.nextType;
//   int32_t nextOffset = info.nextOffset;
//   const char *nextOffsetSign = getSign(nextOffset);
//   int32_t nextOffsetValue = abs(nextOffset);
//
//   switch(basicType) {
//     case BASIC_TYPE_INT: {
//       switch(nextType.basicType) {
//         case BASIC_TYPE_INT: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME INT %s -> INT %s ---\n",
//             nextData->variableName, variableName
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov rax, [rbp %s %" PRIi32 "]\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], rax\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         case BASIC_TYPE_BOOL: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME BOOL %s -> INT %s ---\n",
//             nextData->variableName, variableName
//           );
//
//           fprintf(
//             compilerOptions->output,
//             "movzx eax, BYTE [rbp %s %" PRIi32 "]\n",
//             // We can use EAX, as it zeroes out the whole RAX for some reason
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], rax\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         case BASIC_TYPE_CHAR: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME CHAR %s -> INT %s ---\n",
//             nextData->variableName, variableName
//           );
// 
//           fprintf(
//             compilerOptions->output,
//             "mov eax, DWORD [rbp %s %" PRIi32 "]\n",
//             // We can use EAX, as it zeroes out the whole RAX for some reason
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], rax\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         default: {
//           ASSERT(false, "Type not supported!");
//         }
//       }
//       break;
//     }
//     case BASIC_TYPE_BOOL: {
//       switch(nextType.basicType) {
//         case BASIC_TYPE_BOOL: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME BOOL %s -> BOOL %s ---\n",
//             nextData->variableName, variableName
//           );
//
//           fprintf(
//             compilerOptions->output,
//             "mov al, [rbp %s %" PRIi32 "]\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], al\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         case BASIC_TYPE_INT: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME INT %s -> BOOL %s ---\n",
//             nextData->variableName, variableName
//           );
//           fputs("xor rbx, rbx\n", compilerOptions->output);
//           fprintf(
//             compilerOptions->output,
//             "cmp QWORD [rbp %s %" PRIi32 "], 0\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fputs("setne bl\n", compilerOptions->output);
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], bl\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         case BASIC_TYPE_CHAR: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME CHAR %s -> BOOL %s ---\n",
//             nextData->variableName, variableName
//           );
//           fputs("xor rbx, rbx\n", compilerOptions->output);
//           fprintf(
//             compilerOptions->output,
//             "cmp DWORD [rbp %s %" PRIi32 "], 0\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fputs("setne bl\n", compilerOptions->output);
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], bl\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         default: {
//           ASSERT(false, "Type not supported!");
//         }
//       }
//       break;
//     }
//     case BASIC_TYPE_FUNCTION: {
//       switch(nextType.basicType) {
//         case BASIC_TYPE_FUNCTION: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME FUNCTION %s -> FUNCTION %s ---\n",
//             nextData->variableName, variableName
//           );
//
//           fprintf(
//             compilerOptions->output,
//             "mov rax, [rbp %s %" PRIi32 "]\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], rax\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         default: {
//           ASSERT(false, "Type not supported!");
//         }
//       }
//       break;
//     }
//     case BASIC_TYPE_CHAR: {
//       switch(nextType.basicType) {
//         case BASIC_TYPE_CHAR: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME CHAR %s -> CHAR %s ---\n",
//             nextData->variableName, variableName
//           );
//
//           fprintf(
//             compilerOptions->output,
//             "mov eax, DWORD [rbp %s %" PRIi32 "]\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], eax\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         case BASIC_TYPE_INT: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME INT %s -> CHAR %s ---\n",
//             nextData->variableName, variableName
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov rax, [rbp %s %" PRIi32 "]\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], eax\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         case BASIC_TYPE_BOOL: {
//           fprintf(
//             compilerOptions->output,
//             "; --- ASSIGN NAME BOOL %s -> CHAR %s ---\n",
//             nextData->variableName, variableName
//           );
//
//           fprintf(
//             compilerOptions->output,
//             "mov eax, BYTE [rbp %s %" PRIi32 "]\n",
//             nextOffsetSign,
//             nextOffsetValue
//           );
//           fprintf(
//             compilerOptions->output,
//             "mov [rbp %s %" PRIi32 "], eax\n",
//             offsetSign,
//             offsetValue
//           );
//           break;
//         }
//         default: {
//           printf("%d %s\n", nextType.basicType, getBasicTypeName(nextType.basicType));
//           ASSERT(false, "Type not supported!");
//         }
//       }
//       break;
//     }
//     case BASIC_TYPE_ARRAY: {
//       printf("nextType: %d %s\n", nextType.basicType, getBasicTypeName(nextType.basicType));
//       if(!canTypesConvert(type, nextType)) {
//         ASSERT(false, "Type not supported!");
//       }
//       ArrayType *fromArrayType = nextType.data;
//       Type fromElementType = fromArrayType->type;
//
//       ArrayType *arrayType = type.data;
//       Type elementType = arrayType->type;
//
//       // int32_t newOffset = offset > 0 ? offset - getTypeByteOffset(type) : offset + getTypeByteOffset(type);
//       int32_t newOffset = offset;
//       printf("offset: %" PRIi32 " %" PRIi32 "\n", newOffset, offset);
//       // int32_t newNextOffset = nextOffset > 0 ? nextOffset - getTypeByteOffset(type) : nextOffset + getTypeByteOffset(type);
//       int32_t newNextOffset = nextOffset;
//       printf("offset: %" PRIi32 " %" PRIi32 "\n", newNextOffset, nextOffset);
//       for(size_t i = 0;i < arrayType->numberOfElements;i++) {
//         generateNameAssignAsm((GenerateNameAssignAsmInfo) {
//           .compilerOptions = info.compilerOptions,
//           .nextData = nextData,
//           .nextType = fromElementType,
//           .offset = newOffset,
//           .program = info.program,
//           .type = elementType,
//           .variableName = info.variableName,
//           .nextOffset = newNextOffset,
//         });
//         newOffset = newOffset > 0 ? newOffset + getTypeByteOffset(elementType) : newOffset - getTypeByteOffset(elementType);
//         newNextOffset = newNextOffset > 0 ? newNextOffset + getTypeByteOffset(elementType) : newNextOffset - getTypeByteOffset(elementType);
//       }
//       int32_t check = offset < 0 ? offset - getTypeByteOffset(type) : offset + getTypeByteOffset(type);
//       if(newOffset != check) {
//         printf("newOffset: %" PRIi32 " %" PRIi32 " %" PRIi32 "\n", newOffset, offset, check);
//         ASSERT(false, "Wat?");
//       }
//       int32_t checkNext = nextOffset < 0 ? nextOffset - getTypeByteOffset(type) : nextOffset + getTypeByteOffset(type);
//       if(newNextOffset != checkNext) {
//         printf("newOffset: %" PRIi32 " %" PRIi32 " %" PRIi32 "\n", newNextOffset, nextOffset, checkNext);
//         ASSERT(false, "Wat?");
//       }
//       break;
//     }
//     default: {
//       ASSERT(false, "Type not supported!");
//       break;
//     }
//   }
// }

void generateAssignAsm(GenerateAssignAsmInfo info) {
  Program *program = info.program;
  CompilerOptions *compilerOptions = info.compilerOptions;
  const char *variableName = info.variableName;
  Type type = info.type;
  BasicType basicType = type.basicType;

  Token *next = info.next;
  int32_t offset = info.offset;
  const char *offsetSign = getSign(offset);
  int32_t offsetValue = abs(offset);

  if(isOperationTokenType(next->type)) {
    generateBinaryOperationAsm(compilerOptions, program, next);
    BinaryOperationData *binaryOperationData = next->data;
    Type operationType = binaryOperationData->type;

    switch(basicType) {
      case BASIC_TYPE_INT: {
        switch(operationType.basicType) {
          case BASIC_TYPE_INT: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION INT -> INT %s ---\n",
              variableName
            );
            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rax\n",
              offsetSign,
              offsetValue
            );
            break;
          }
          case BASIC_TYPE_BOOL: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION BOOL -> INT %s ---\n",
              variableName
            );

            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rax\n",
              offsetSign,
              offsetValue
            );
            break;
          }
          default: {
            ASSERT(false, "Type not supported!");
          }
        }
        break;
      }
      case BASIC_TYPE_BOOL: {
        switch(operationType.basicType) {
          case BASIC_TYPE_BOOL: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION BOOL -> BOOL %s ---\n",
              variableName
            );

            fputs("and rax, 1\n", compilerOptions->output);
            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rax\n",
              offsetSign,
              offsetValue
            );
            break;
          }
          case BASIC_TYPE_INT: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION INT -> BOOL %s ---\n",
              variableName
            );
            fputs("xor rbx, rbx\n", compilerOptions->output);
            fputs("cmp rax, 0\n", compilerOptions->output);
            fputs("setne bl\n", compilerOptions->output);
            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rbx\n",
              offsetSign,
              offsetValue
            );
            break;
          }
          default: {
            ASSERT(false, "Type not supported!");
          }
        }
        break;
      }
      default: {
        ASSERT(false, "Type not supported!");
        break;
      }
    }

    return;
  }

  switch (next->type) {
    case TOKEN_VALUE:
    case TOKEN_NAME:
    case TOKEN_INDEX: {
      generateVariableAssignAsm(
        compilerOptions,
        program,
        next,
        info.current
      );
      break;
    }
    case TOKEN_FUNCTION_CALL: {
      generateFunctionCallAsm(compilerOptions, program, next);

      FunctionCallData *functionCallData = next->data;
      const char *name = getFunctionNameFromCall(functionCallData);
      Type returnType = getFunctionReturnTypeFromCall(functionCallData);

      switch(basicType) {
        case BASIC_TYPE_INT: {
          switch(returnType.basicType) {
            case BASIC_TYPE_INT: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN INT FUNCTION %s -> INT %s ---\n",
                name,
                variableName
              );

              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], rax\n",
                offsetSign,
                offsetValue
              );
              break;
            }
            case BASIC_TYPE_BOOL: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN BOOL FUNCTION %s -> INT %s ---\n",
                name,
                variableName
              );

              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], rax\n",
                offsetSign,
                offsetValue
              );
              break;
            }
            default: {
              ASSERT(false, "Type not supported!");
            }
          }
          break;
        }
        case BASIC_TYPE_BOOL: {
          switch(returnType.basicType) {
            case BASIC_TYPE_BOOL: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN BOOL FUNCTION %s -> BOOL %s ---\n",
                name,
                variableName
              );

              fputs("and rax, 1\n", compilerOptions->output);
              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], rax\n",
                offsetSign,
                offsetValue
              );
              break;
            }
            case BASIC_TYPE_INT: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN BOOL FUNCTION %s -> BOOL %s ---\n",
                name,
                variableName
              );

              fputs("and rax, 1\n", compilerOptions->output);
              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], rax\n",
                offsetSign,
                offsetValue
              );
              break;
            }
            default: {
              ASSERT(false, "Type not supported!");
            }
          }
          break;
        }
        default: {
          ASSERT(false, "Type not supported!");
          break;
        }
      }
      break;
    }
    default: {
      fprintf(stderr, "Error: Token type `%s` not implemented in generateAssignAsm!\n", getTokenTypeName(next->type));
      ASSERT(false, "Error: Token not implemented in generateAssignAsm!\n");
      break;
    }
  }

  // switch (next->type) {
  //   case TOKEN_VALUE: {
  //     ValueData *valueData = next->data;
  //
  //     switch(basicType) {
  //       case BASIC_TYPE_INT: {
  //         switch(valueData->type.basicType) {
  //           case BASIC_TYPE_INT: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN INT VALUE %" PRIu64 " -> INT %s ---\n",
  //               *((uint64_t*) valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov QWORD [rbp %s %" PRIi32 "], %" PRIu64 "\n",
  //               offsetSign,
  //               offsetValue,
  //               *((uint64_t*) valueData->data)
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_BOOL: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN BOOL VALUE %" PRIu8 " -> INT %s ---\n",
  //               *((uint8_t*) valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov QWORD [rbp %s %" PRIi32 "], %" PRIu8 "\n",
  //               offsetSign,
  //               offsetValue,
  //               *((uint8_t*) valueData->data)
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_CHAR: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN CHAR VALUE %" PRIu32 " -> INT %s ---\n",
  //               getCharValue(valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov QWORD [rbp %s %" PRIi32 "], %" PRIu32 "\n",
  //               offsetSign,
  //               offsetValue,
  //               getCharValue(valueData->data)
  //             );
  //             break;
  //           }
  //           default: {
  //             ASSERT(false, "Type not supported!");
  //           }
  //         }
  //         break;
  //       }
  //       case BASIC_TYPE_BOOL: {
  //         switch(valueData->type.basicType) {
  //           case BASIC_TYPE_BOOL: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN BOOL VALUE %s -> BOOL %s ---\n",
  //               getBoolStringFromValue(valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
  //               offsetSign,
  //               offsetValue,
  //               *((uint8_t*) valueData->data)
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_INT: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN INT VALUE %" PRIu64 " (Normalized: %" PRIu8 ") -> BOOL %s ---\n",
  //               *((uint64_t*) valueData->data),
  //               getNormalizedBoolValueFromUInt64(valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
  //               offsetSign,
  //               offsetValue,
  //               getNormalizedBoolValueFromUInt64(valueData->data)
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_CHAR: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN CHAR VALUE %" PRIu32 " (Normalized: %" PRIu8 ") -> BOOL %s ---\n",
  //               getCharValue(valueData->data),
  //               getNormalizedBoolValueFromUInt32(valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
  //               offsetSign,
  //               offsetValue,
  //               getNormalizedBoolValueFromUInt32(valueData->data)
  //             );
  //             break;
  //           }
  //           default: {
  //             ASSERT(false, "Type not supported!");
  //           }
  //         }
  //         break;
  //       }
  //       case BASIC_TYPE_FUNCTION: {
  //         switch(valueData->type.basicType) {
  //           case BASIC_TYPE_FUNCTION: {
  //             FunctionTypeData *ftd = valueData->data;
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN FUNCTION VALUE %s -> FUNCTION %s ---\n",
  //               ftd->name,
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov rax, %s\n",
  //               ftd->name
  //             );
  //             fprintf(
  //               compilerOptions->output,
  //               "mov [rbp %s %" PRIi32 "], rax\n",
  //               offsetSign,
  //               offsetValue
  //             );
  //             break;
  //           }
  //           default: {
  //             ASSERT(false, "Type not supported!");
  //           }
  //         }
  //         break;
  //       }
  //       case BASIC_TYPE_CHAR: {
  //         switch(valueData->type.basicType) {
  //           case BASIC_TYPE_CHAR: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN CHAR VALUE %" PRIu32 " -> CHAR %s ---\n",
  //               getCharValue(valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov DWORD [rbp %s %" PRIi32 "], %" PRIu32 "\n",
  //               offsetSign,
  //               offsetValue,
  //               getCharValue(valueData->data)
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_BOOL: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN BOOL VALUE %s -> CHAR %s ---\n",
  //               getBoolStringFromValue(valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov DWORD [rbp %s %" PRIi32 "], %" PRIu8 "\n",
  //               offsetSign,
  //               offsetValue,
  //               getBoolValue(valueData->data)
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_INT: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN INT VALUE %" PRIu64 " (Normalized: %" PRIu32 ") -> CHAR %s ---\n",
  //               getIntValue(valueData->data),
  //               getNormalizedCharValueFromUInt64(valueData->data),
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov DWORD [rbp %s %" PRIi32 "], %" PRIu32 "\n",
  //               offsetSign,
  //               offsetValue,
  //               getNormalizedCharValueFromUInt64(valueData->data)
  //             );
  //             break;
  //           }
  //           default: {
  //             ASSERT(false, "Type not supported!");
  //           }
  //         }
  //         break;
  //       }
  //       case BASIC_TYPE_ARRAY: {
  //         switch(valueData->type.basicType) {
  //           case BASIC_TYPE_ARRAY: {
  //             if(!canTypesConvert(type, valueData->type)) {
  //               ASSERT(false, "Type not supported!");
  //             }
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN ARRAY VALUE -> ARRAY %s ---\n",
  //               variableName
  //             );
  //             List *list = valueData->data;
  //
  //             ArrayType *nameArrayType = type.data;
  //             Type nameElementType = nameArrayType->type;
  //
  //             offsetValue = offsetValue - getTypeByteOffset(valueData->type);
  //             for(size_t i = 0;i < list->size;i++) {
  //               Token *token = list->elements[i];
  //
  //               int32_t newOffset = (offset < 0 ? -1 : 1) * (offsetValue + (int32_t) getTypeByteSize(nameElementType));
  //               generateAssignAsm((GenerateAssignAsmInfo) {
  //                 .compilerOptions = info.compilerOptions,
  //                 .next = token,
  //                 .offset = newOffset,
  //                 .program = info.program,
  //                 .type = nameElementType,
  //                 .variableName = info.variableName,
  //               });
  //               offsetValue = offsetValue + getTypeByteOffset(nameElementType);
  //             }
  //             if(offsetValue != abs(offset)) {
  //               ASSERT(false, "Wat?");
  //             }
  //
  //             break;
  //           }
  //           default: {
  //             ASSERT(false, "Type not supported!");
  //           }
  //         }
  //         break;
  //       }
  //       default: {
  //         ASSERT(false, "Type not supported!");
  //         break;
  //       }
  //     }
  //     break;
  //   }
  //   case TOKEN_NAME: {
  //     NameData *nextData = info.next->data;
  //     generateNameAssignAsm((GenerateNameAssignAsmInfo) {
  //       .compilerOptions = info.compilerOptions,
  //       .nextData = nextData,
  //       .nextType = *nextData->type,
  //       .nextOffset = calculateOffset(program, nextData),
  //       .offset = info.offset,
  //       .program = info.program,
  //       .type = info.type,
  //       .variableName = info.variableName,
  //     });
  //     break;
  //   }
  //   case TOKEN_INDEX: {
  //     IndexData *indexData = next->data;
  //     NameData *nextData = indexData->nameData;
  //
  //     ArrayType *arrayType = nextData->type->data;
  //
  //     generateNameAssignAsm((GenerateNameAssignAsmInfo) {
  //       .compilerOptions = info.compilerOptions,
  //       .nextData = nextData,
  //       .nextType = arrayType->type,
  //       .nextOffset = calculateOffset(program, nextData),
  //       .offset = info.offset,
  //       .program = info.program,
  //       .type = info.type,
  //       .variableName = info.variableName,
  //     });
  //     break;
  //   }
  //   case TOKEN_FUNCTION_CALL: {
  //     generateFunctionCallAsm(compilerOptions, program, next);
  //
  //     FunctionCallData *functionCallData = next->data;
  //     const char *name = getFunctionNameFromCall(functionCallData);
  //     Type returnType = getFunctionReturnTypeFromCall(functionCallData);
  //
  //     switch(basicType) {
  //       case BASIC_TYPE_INT: {
  //         switch(returnType.basicType) {
  //           case BASIC_TYPE_INT: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN INT FUNCTION %s -> INT %s ---\n",
  //               name,
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov [rbp %s %" PRIi32 "], rax\n",
  //               offsetSign,
  //               offsetValue
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_BOOL: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN BOOL FUNCTION %s -> INT %s ---\n",
  //               name,
  //               variableName
  //             );
  //
  //             fprintf(
  //               compilerOptions->output,
  //               "mov [rbp %s %" PRIi32 "], rax\n",
  //               offsetSign,
  //               offsetValue
  //             );
  //             break;
  //           }
  //           default: {
  //             ASSERT(false, "Type not supported!");
  //           }
  //         }
  //         break;
  //       }
  //       case BASIC_TYPE_BOOL: {
  //         switch(returnType.basicType) {
  //           case BASIC_TYPE_BOOL: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN BOOL FUNCTION %s -> BOOL %s ---\n",
  //               name,
  //               variableName
  //             );
  //
  //             fputs("and rax, 1\n", compilerOptions->output);
  //             fprintf(
  //               compilerOptions->output,
  //               "mov [rbp %s %" PRIi32 "], rax\n",
  //               offsetSign,
  //               offsetValue
  //             );
  //             break;
  //           }
  //           case BASIC_TYPE_INT: {
  //             fprintf(
  //               compilerOptions->output,
  //               "; --- ASSIGN BOOL FUNCTION %s -> BOOL %s ---\n",
  //               name,
  //               variableName
  //             );
  //
  //             fputs("and rax, 1\n", compilerOptions->output);
  //             fprintf(
  //               compilerOptions->output,
  //               "mov [rbp %s %" PRIi32 "], rax\n",
  //               offsetSign,
  //               offsetValue
  //             );
  //             break;
  //           }
  //           default: {
  //             ASSERT(false, "Type not supported!");
  //           }
  //         }
  //         break;
  //       }
  //       default: {
  //         ASSERT(false, "Type not supported!");
  //         break;
  //       }
  //     }
  //     break;
  //   }
  //   default: {
  //     fprintf(stderr, "Error: Token type `%s` not implemented in generateAssignAsm!\n", getTokenTypeName(next->type));
  //     ASSERT(false, "Error: Token not implemented in generateAssignAsm!\n");
  //     break;
  //   }
  // }
}

NameData *getProgramVariable(Program *program, const char* name) {
  if(!existsElementInHashTable(program->variableMap, name)) {
    if(program->parent) {
      return getProgramVariable(program->parent, name);
    }
    return NULL;
  }
  return getElementFromHashTable(program->variableMap, name);
}

void generateValueAsm(CompilerOptions *compilerOptions, Token *token, Register destination) {
  ValueData *data = token->data;
  switch(data->type.basicType) {
    case BASIC_TYPE_BOOL: {
      fprintf(
        compilerOptions->output,
        "; --- TOKEN VALUE BOOL %" PRIu8 " (Normalized: %" PRIu8 ") ---\n",
        *((uint8_t*) data->data),
        getNormalizedBoolValueFromUInt8(data->data)
      );

      fprintf(
        compilerOptions->output,
        "mov %s, %" PRIu8 "\n",
        get32BitRegister(destination),
        // Setting EAX clears RAX
        getNormalizedBoolValueFromUInt8(data->data)
      );
      break;
    }
    case BASIC_TYPE_INT: {
      fprintf(
        compilerOptions->output,
        "; --- TOKEN VALUE INT %" PRIu64 " ---\n",
        *((uint64_t*) data->data)
      );

      fprintf(
        compilerOptions->output,
        "mov %s, %" PRIu64 "\n",
        get64BitRegister(destination),
        *((uint64_t*) data->data)
      );
      break;
    }
    case BASIC_TYPE_CHAR: {
      fprintf(
        compilerOptions->output,
        "; --- TOKEN VALUE CHAR %" PRIu32 " ---\n",
        getCharValue(data->data)
      );

      fprintf(
        compilerOptions->output,
        "mov %s, %" PRIu32 "\n",
        get32BitRegister(destination),
        getCharValue(data->data)
      );
      break;
    }
    default: {
      ASSERT(false, "Type not supported!");
    }
  }
}

void generateIndexAsm(CompilerOptions *compilerOptions, Program *program, Token *token, Register destination) {
  IndexData *data = token->data;
  NameData *nameData = data->nameData;
  ArrayType *arrayType = nameData->type->data;
  Type type = arrayType->type;

  int32_t offset = calculateOffset(program, nameData);
  const char *sign = getSign(offset);
  int32_t offsetValue = abs(offset);

  fprintf(
    compilerOptions->output,
    "; --- TOKEN INDEX %s ---\n",
    nameData->variableName
  );

  if(data->index->type == TOKEN_VALUE) {
    fprintf(
      compilerOptions->output,
      "mov %s, [rbp %s %" PRIi32 "]\n",
      getRegisterBySize(destination, type),
      sign,
      offsetValue
    );
  } else if(data->index->type == TOKEN_NAME) {
    size_t size = getTypeByteSize(type);
    if(size <= 8) {
      offset = offset < 0 ? offset - 8 + size : offset + 8 - size;
    }
    // if(offset >= 0) {
    //   offset += getTypeByteOffset(*nameData->type) - 8;
    // }
    sign = getSign(offset);
    offsetValue = abs(offset);

    NameData *indexName = data->index->data;
    if(indexName->type->basicType != BASIC_TYPE_INT) {
      ASSERT(false, "Unreachable!");
    }

    generateNameAsm(compilerOptions, program, data->index, REGISTER_A);
    fprintf(
      compilerOptions->output,
      "mov %s, [rbp %s %" PRIi32 " + rax * %zu]\n",
      getRegisterBySize(destination, type),
      sign,
      offsetValue,
      getTypeByteOffset(type)
    );
  } else {
    ASSERT(false, "Unreachable!");
  }

}

void generateNameAsm(CompilerOptions *compilerOptions, Program *program, Token *token, Register destination) {
  NameData *data = token->data;
  int32_t offset = calculateOffset(program, data);
  const char *sign = getSign(offset);
  int32_t offsetValue = abs(offset);

  fprintf(
    compilerOptions->output,
    "; --- TOKEN NAME %s ---\n",
    data->variableName
  );

  fprintf(
    compilerOptions->output,
    "mov %s, [rbp %s %" PRIi32 "]\n",
    getRegisterBySize(destination, *data->type),
    sign,
    offsetValue
  );
  return;

  switch(data->type->basicType) {
    case BASIC_TYPE_INT: {
      fprintf(
        compilerOptions->output,
        "mov %s, [rbp %s %" PRIi32 "]\n",
        get64BitRegister(destination),
        getSign(offset),
        abs(offset)
      );
      break;
    }
    case BASIC_TYPE_BOOL: {
      fprintf(
        compilerOptions->output,
        "movzx %s, BYTE [rbp %s %" PRIi32 "]\n",
        get32BitRegister(destination),
        // We can use EAX, as it zeroes out the whole RAX for some reason
        getSign(offset),
        abs(offset)
      );
      break;
    }
    case BASIC_TYPE_FUNCTION: {
      fprintf(
        compilerOptions->output,
        "mov %s, [rbp %s %" PRIi32 "]\n",
        get64BitRegister(destination),
        getSign(offset),
        abs(offset)
      );
      break;
    }
    default: {
      ASSERT(false, "Type not supported!");
    }
  }
}

void generateScopeAsm(CompilerOptions *compilerOptions, Program *program) {
  fprintf(
    compilerOptions->output,
    "; --- SCOPE ---\n"
  );
  fputs("push rbp\n", compilerOptions->output);
  fputs("mov rbp, rsp\n", compilerOptions->output);
  fprintf(compilerOptions->output, "sub rsp, %" PRIi32 "\n", -program->variableOffset);

  generateProgramAsm(compilerOptions, program);

  fputs("mov rsp, rbp\n", compilerOptions->output);
  fputs("pop rbp\n", compilerOptions->output);
}

void generateIfAsm(CompilerOptions *compilerOptions, Program *program, size_t i, ControlFlowBlock *data) {
  Program p = {
    .capacity = 1,
    .count = 1,
    .instructions = &data->condition,
    .parent = program,
    .id = PROGRAM_COUNT++,
  };
  generateProgramAsm(compilerOptions, &p);
  fprintf(compilerOptions->output, "cmp rax, 0\n");
  fprintf(compilerOptions->output, "jz point_%zu_%zu\n", program->id, i + 1);

  generateScopeAsm(compilerOptions, data->program);
  fprintf(compilerOptions->output, "jmp point_%zu_%zu\n", program->id, i + data->endInstruction);
  fprintf(compilerOptions->output, "point_%zu_%zu:\n", program->id, i + 1);
}

void generateElseAsm(CompilerOptions *compilerOptions, Program *program, size_t i, ControlFlowBlock *data) {
  generateScopeAsm(compilerOptions, data->program);
  fprintf(compilerOptions->output, "point_%zu_%zu:\n", program->id, i + 1);
}

void generateProgramAsm(CompilerOptions *compilerOptions, Program *program) {
  if(program->functions && program->functions->size) {
    for(size_t i = 0; i < program->functions->capacity;i++) {
      const char *functionName = program->functions->elements[i].key;
      if(!functionName) continue;
      FunctionDefinition *data = program->functions->elements[i].value;
      generateFunctionAsm(compilerOptions, data);
    }
  }

  // for(size_t i = 0;i < program->variables->size;i++) {
  //   NameData *nd = program->variables->elements[i];
  //   if(!nd || !nd->offset) continue;
  //   printf("Nd: %s %s %" PRIi32 "\n", nd->variableName, nd->name, *nd->offset);
  // }

  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    // printToken(token, 0, i);

    if(isOperationTokenType(token->type)) {
      generateBinaryOperationAsm(compilerOptions, program, token);
      continue;
    }

    switch(token->type) {
      case TOKEN_NAME: {
        NameData *data = token->data;
        fprintf(
          compilerOptions->output,
          "; --- TOKEN NAME %s ---\n",
          data->variableName
        );

        Token *next = program->instructions[++i];
        if(next->type == TOKEN_ASSIGN) {
          generateAssignAsm((GenerateAssignAsmInfo) {
            .compilerOptions = compilerOptions,
            .next = nextToken(program, &i),
            .offset = calculateOffset(program, data),
            .program = program,
            .type = *data->type,
            .variableName = data->variableName,
            .current = token,
          });
        } else {
          generateNameAsm(compilerOptions, program, token, REGISTER_A);
          i--;
        }
        break;
      }
      case TOKEN_VALUE: {
        generateValueAsm(compilerOptions, token, REGISTER_A);
        break;
      }
      case TOKEN_PRINT: {
        Token *child = token->data;
        switch(child->type) {
          case TOKEN_NAME: {
            NameData *data = child->data;
            ASSERT(data, "Unreachable!");
            fprintf(
              compilerOptions->output,
              "; --- TOKEN PRINT NAME %s ---\n",
              data->variableName
            );
            int32_t offset = calculateOffset(program, data);
            const char *offsetSign = getSign(offset);
            int32_t offsetValue = abs(offset);

            if(data->type->basicType == BASIC_TYPE_CHAR) {
              fprintf(
                compilerOptions->output,
                "mov edi, [rbp %s %" PRIi32 "]\n",
                offsetSign,
                offsetValue
              );
              fputs("call printChar\n", compilerOptions->output);
              break;
            } else {
              fprintf(
                compilerOptions->output,
                "mov rdi, [rbp %s %" PRIi32 "]\n",
                offsetSign,
                offsetValue
              );
              fputs("call print64\n", compilerOptions->output);
            }
            break;
          }
          case TOKEN_VALUE: {
            fprintf(
              compilerOptions->output,
              "; --- TOKEN PRINT VALUE ---\n"
            );
            generateValueAsm(compilerOptions, child, REGISTER_DI);
            ValueData *valueData = child->data;
            if(valueData->type.basicType == BASIC_TYPE_CHAR) {
              fputs("call printChar\n", compilerOptions->output);
            } else {
              fputs("call print64\n", compilerOptions->output);
            }
            break;
          }
          case TOKEN_INDEX: {
            IndexData *indexData = child->data;
            NameData *data = indexData->nameData;
            ASSERT(data, "Unreachable!");
            fprintf(
              compilerOptions->output,
              "; --- TOKEN PRINT NAME %s ---\n",
              data->variableName
            );
            ArrayType *arrayType = data->type->data;

            generateIndexAsm(
              compilerOptions,
              program,
              child,
              REGISTER_DI
            );
            
            if(arrayType->type.basicType == BASIC_TYPE_CHAR) {
              fputs("call printChar\n", compilerOptions->output);
            } else {
              fputs("call print64\n", compilerOptions->output);
            }
            break;
          }
          default: {
            ASSERT(false, "Type not supported!");
          }
        }

        break;
      }
      case TOKEN_RETURN: {
        TokenPriorityData *data = token->data;
        ASSERT(data->count <= 1, "Not implemented yet!");
        if(data->count == 0) {
          fputs("; --- TOKEN RETURN EMPTY ---\n", compilerOptions->output);
        } else if(data->count == 1) {
          fputs("; --- TOKEN RETURN ONE ---\n", compilerOptions->output);

          Program p = {
            .capacity = data->count,
            .count = data->count,
            .instructions = data->instructions,
            .functions = NULL,
            .id = 0,
            .parent = program,
            .variableOffset = 0,
            // .variables = program->variables,
            .variables = NULL,
            .useInOffsetCalculations = false
          };
          generateProgramAsm(compilerOptions, &p);
        } else {
          ASSERT(false, "Not implemented yet");
        }
        
        size_t numberOfParents = 0;
        for(Program *p = program, *last = NULL;p != NULL;last = p, p = p->parent, numberOfParents++) {
          if(!p->functions) continue;
          for(size_t fnci = 0;fnci < p->functions->capacity;fnci++) {
            if(!p->functions->elements[fnci].key) continue;
            FunctionDefinition *fd = p->functions->elements[fnci].value;
            if(fd->body == last) {
              goto parentCounterLoopEnd;
            }
          }
          continue;
          parentCounterLoopEnd:
          break;
        }

        for(size_t i = 0;i < numberOfParents;i++) {
          fputs("mov rsp, rbp\n", compilerOptions->output);
          fputs("pop rbp\n", compilerOptions->output);
        }
        fputs("ret\n", compilerOptions->output);
        break;
      }
      case TOKEN_SEMICOLON: {
        // TODO: What should it do?
        break;
      }
      case TOKEN_FUNCTION_CALL: {
        generateFunctionCallAsm(compilerOptions, program, token);
        break;
      }
      case TOKEN_SCOPE: {
        generateScopeAsm(compilerOptions, token->data);
        break;
      }
      case TOKEN_IF: {
        generateIfAsm(compilerOptions, program, i, token->data);
        break;
      }
      case TOKEN_ELSE: {
        generateElseAsm(compilerOptions, program, i, token->data);
        break;
      }
      default: {
        fprintf(stderr, "Error: Token type `%s` not implemented in compilation!\n", getTokenTypeName(token->type));
        fprintf(stderr, "ERROR at: %s:%zu:%zu\n",
          token->file, token->line, token->column);
        ASSERT(false, "Error: Token not implemented in generateAssignAsm!\n");
        exit(1);
        break;
      }
    }
  }
}

void generateAsm(CompilerOptions *compilerOptions) {
  ASSERT(TOKEN_COUNT == 31, "Not all operations are implemented in compile!");

  char *asmName = calloc(strlen(compilerOptions->basename) + 4 + 1, sizeof(char));
  sprintf(asmName, "%s.asm", compilerOptions->basename);
  FILE *out = openFile(asmName, "w");
  compilerOptions->output = out;
  if(!compilerOptions->silent) printf("[INFO]: Generating %s\n", asmName);

  prepareFileForCompile(out);
  HashTable *globalVariables = createHashTable(256);

  generateProgramAsm(compilerOptions, compilerOptions->program);
  postCompile(compilerOptions);

  bool data = false, bss = false;

  (void) data;
  (void) bss;

  for(size_t i = 0;i < globalVariables->capacity;i++) {
    if(globalVariables->elements[i].key == NULL) continue;
    const char* name = globalVariables->elements[i].key;

    (void) name;
  }
  for(size_t i = 0;i < globalVariables->capacity;i++) {
    if(globalVariables->elements[i].key == NULL) continue;
    const char* name = globalVariables->elements[i].key;

    (void) name;
  }

  fclose(out);
}
void compile(CompilerOptions *compilerOptions) {
  // TODO: Check if folder exists for output
  // TODO: And create if it doesn't exist

  char call[128];
  snprintf(call, 128, "nasm -g -felf64 %s.asm", compilerOptions->basename);
  if(!compilerOptions->silent) printf("[CMD]: %s\n", call);
  system(call);

  snprintf(call, 128, "ld -o %s %s.o", compilerOptions->basename, compilerOptions->basename);
  if(!compilerOptions->silent) printf("[CMD]: %s\n", call);
  system(call);
}
void runProgram(CompilerOptions *compilerOptions) {
  char call[128];
  snprintf(call, 128, "./%s", compilerOptions->basename);
  if(!compilerOptions->silent) printf("[CMD]: %s\n", call);
  system(call);
}
