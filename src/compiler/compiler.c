#include "compiler.h"
#include "../utils/utils.h"

// #region Registers
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

const char *getRegisterBySize(Register reg, Type *type) {
  switch(getTypeByteSize(type)) {
    case 1: return get8BitRegister(reg);
    case 2: return get16BitRegister(reg);
    case 4: return get32BitRegister(reg);
    case 8: return get64BitRegister(reg);
    default: ASSERT(false, "Unknown Type!");
  }
}
// #endregion

// #region Data Size
const char *getAsmSizeByType(Type *type) {
  switch(getTypeByteSize(type)) {
    case 1: return "BYTE";
    case 2: return "WORD";
    case 4: return "DWORD";
    case 8: return "QWORD";
    default: ASSERT(false, "Unknown Type!");
  }
}
// #endregion

// #region Build-in Function
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
// #endregion

// #region Boilerplate
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
// #endregion

void generateFunctionAsm(CompilerOptions *compilerOptions, FunctionDefinition *functionData) {
  // TODO: Optimize to use register for some fields instead of stack because faster 👍
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

bool extractOnlyFromPriority(Token **pToken) {
  if(!pToken) return false;
  if(!*pToken) return false;
  if((*pToken)->type != TOKEN_PRIORITY) return false;
  TokenPriorityData *data = (*pToken)->data;
  if(data->count != 1) return false;
  (*pToken) = data->instructions[0];

  return true;
}

void generateBinaryOperationAsm(CompilerOptions *compilerOptions, Program *program, Token *operationToken) {
  BinaryOperationData *data = operationToken->data;
  Token *left = data->operandOne, *right = data->operandTwo;
  fprintf(
    compilerOptions->output,
    "; --- BINARY OPERATION %s | TYPE %s ---\n",
    getTokenTypeName(operationToken->type),
    getTypeName(&data->type)
  );

  switch (left->type) {
    case TOKEN_VALUE: {
      generateValueAsm(compilerOptions, left, REGISTER_A);
      break;
    }
    case TOKEN_NAME: {
      generateNameAsm(compilerOptions, program, left->data, REGISTER_A);
      break;
    }
    default: {
      while(extractOnlyFromPriority(&left)) {}
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
      generateValueAsm(compilerOptions, right, REGISTER_B);
      break;
    }
    case TOKEN_NAME: {
      generateNameAsm(compilerOptions, program, right->data, REGISTER_B);
      break;
    }
    default: {
      while(extractOnlyFromPriority(&right)) {}
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
        .variableMap = program->variableMap,
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
    getTypeName(&data->type)
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
      bytes += getTypeByteOffset(nameData->type);
    } else if(arg->type == TOKEN_INDEX) {
      IndexData *indexData = arg->data;
      NameData *nameData = indexData->nameData;
      ArrayType *arrayType = nameData->type->data;

      bytes += getTypeByteOffset(&arrayType->type);
    } else if(arg->type == TOKEN_VALUE) {
      ValueData *valueData = arg->data;
      bytes += getTypeByteOffset(&valueData->type);
    } else if(isOperationTokenType(arg->type)) {
      BinaryOperationData *data = arg->data;
      bytes += getTypeByteOffset(&data->type);
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
        size_t parameterSize = getTypeByteSize(parameterTypes[i]);

        int32_t offset = calculateOffset(&p, nameData);
        generateAssignVariableToVariableAsm(
          compilerOptions,
          parameterSize < 8 ? -(bytes - parameterSize) : -(bytes - 8 + parameterSize),
          parameterTypes[i],
          offset,
          nameData->type,
          offset < 0
        );

        bytes += getTypeByteOffset(nameData->type);
      } else if(arg->type == TOKEN_INDEX) {
        IndexData *indexData = arg->data;
        NameData *nextData = indexData->nameData;
        ArrayType *arrayType = nextData->type->data;

        int32_t offset = calculateOffset(&p, nextData);
        generateAssignVariableToVariableAsm(
          compilerOptions,
          -(bytes - getTypeByteSize(&arrayType->type)),
          parameterTypes[i],
          offset,
          &arrayType->type,
          offset < 0
        );

        bytes += getTypeByteOffset(nextData->type);
      } else if(arg->type == TOKEN_VALUE) {
        size_t parameterSize = getTypeByteSize(parameterTypes[i]);

        generateAssignValueToVariableAsm(
          compilerOptions,
          &p,
          parameterSize < 8 ? -(bytes - parameterSize) : -(bytes - 8 + parameterSize),
          parameterTypes[i],
          arg->data,
          false
        );
        // generateAddressAssignAsm(
        //   compilerOptions,
        //   &p,
        //   arg,
        //   *parameterTypes[i],
        //   // -bytes,
        //   parameterSize < 8 ? -(bytes - parameterSize) : -(bytes - 8 + parameterSize),
        //   true
        // );

        ValueData *valueData = arg->data;
        bytes += getTypeByteOffset(&valueData->type);
      } else if(isOperationTokenType(arg->type)) {
        generateBinaryOperationAsm(compilerOptions, &p, arg);
        fprintf(compilerOptions->output, "mov [rbp - %zu], rax\n", bytes);

        BinaryOperationData *data = arg->data;
        bytes += getTypeByteOffset(&data->type);
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
        get8BitRegister(destination),
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
      getRegisterBySize(destination, &type),
      sign,
      offsetValue
    );
  } else if(data->index->type == TOKEN_NAME) {
    size_t size = getTypeByteSize(&type);
    if(size < 8) {
      offset = offset < 0 ? offset - size : offset + size;
    }
    sign = getSign(offset);
    offsetValue = abs(offset);

    NameData *indexName = data->index->data;
    if(indexName->type->basicType != BASIC_TYPE_INT) {
      ASSERT(false, "Unreachable!");
    }

    generateNameAsm(compilerOptions, program, data->index->data, REGISTER_A);
    fprintf(
      compilerOptions->output,
      "mov %s, [rbp %s %" PRIi32 " + rax * %zu]\n",
      getRegisterBySize(destination, &type),
      sign,
      offsetValue,
      getTypeByteOffset(&type)
    );
  } else {
    ASSERT(false, "Unreachable!");
  }

}

void generateNameAsm(CompilerOptions *compilerOptions, Program *program, NameData *data, Register destination) {
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
    getRegisterBySize(destination, data->type),
    sign,
    offsetValue
  );
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

// #region Assign

void generateAssignArrayVariableToArrayVariableAsm(
  CompilerOptions *compilerOptions,
  Type *elementType,
  size_t size,
  int32_t toStart,
  int32_t toEnd,
  int32_t fromStart,
  int32_t fromEnd
) {
  ASSERT(isBasicType(elementType), "Unreachable");

  size_t elementSize = getTypeByteSize(elementType);
  if(elementSize >= 8) elementSize = 0;
  size_t elementOffset = getTypeByteOffset(elementType);

  const char *reg = getRegisterBySize(REGISTER_A, elementType);

  bool directionTo = toStart < toEnd;
  bool directionFrom = fromStart < fromEnd;
  for(size_t i = 0; i < size;i++) {
    // int32_t to = directionTo ? toStart + elementSize : toStart - elementSize;
    // int32_t from = directionFrom ? fromStart + elementSize : fromStart - elementSize;
    int32_t to = toStart + elementSize;
    int32_t from = fromStart + elementSize;

    fprintf(
      compilerOptions->output,
      "mov %s, [rbp %s %" PRIi32 "]\n",
      reg,
      getSign(from),
      abs(from)
    );

    fprintf(
      compilerOptions->output,
      "mov [rbp %s %" PRIi32 "], %s\n",
      getSign(to),
      abs(to),
      reg
    );

    toStart = directionTo ? toStart + elementOffset : toStart - elementOffset;
    fromStart = directionFrom ? fromStart + elementOffset : fromStart - elementOffset;
  }

  ASSERT(toStart == toEnd, "`to` offset not correct");
  ASSERT(fromStart == fromEnd, "`from` offset not correct");
}

void generateAssignVariableToVariableAsm(
  CompilerOptions *compilerOptions,
  int32_t offsetTo,
  Type *typeTo,
  int32_t offsetFrom,
  Type *typeFrom,
  bool reverse
) {
  ASSERT(canTypesConvert(typeFrom, typeTo), "Unreachable");

  const char *sign = getSign(offsetTo);
  int32_t offsetValue = abs(offsetTo);

  if(!isBasicType(typeTo)) {
    if(typeTo->basicType == BASIC_TYPE_ARRAY) {
      size_t arrayOffset = getTypeByteOffset(typeTo);
      ArrayType *arrayType = typeTo->data;
      
      int32_t offsetToEnd = offsetTo + arrayOffset;
      int32_t offsetFromEnd = offsetFrom + arrayOffset;

      generateAssignArrayVariableToArrayVariableAsm(
        compilerOptions,
        &arrayType->type,
        arrayType->numberOfElements,
        reverse ? offsetToEnd : offsetTo,
        reverse ? offsetTo : offsetToEnd,
        offsetFrom,
        offsetFromEnd
      );
      return;
    }
    ASSERT(false, "Not implemented yet!");
  }

  if(areTypesEqual(typeFrom, typeTo)) {
    fprintf(
      compilerOptions->output,
      "mov %s, [rbp %s %" PRIi32 "]\n",
      getRegisterBySize(REGISTER_A, typeFrom),
      getSign(offsetFrom),
      abs(offsetFrom)
    );

    fprintf(
      compilerOptions->output,
      "mov [rbp %s %" PRIi32 "], %s\n",
      sign,
      offsetValue,
      getRegisterBySize(REGISTER_A, typeTo)
    );
    return;
  }

  fprintf(
    compilerOptions->output,
    "mov %s, [rbp %s %" PRIi32 "]\n",
    getRegisterBySize(REGISTER_A, typeFrom),
    getSign(offsetFrom),
    abs(offsetFrom)
  );

  switch(typeTo->basicType) {
    case BASIC_TYPE_INT: {
      switch(typeFrom->basicType) {
        case BASIC_TYPE_BOOL: {
          fputs("movzx rbx, al\n", compilerOptions->output);
          fprintf(
            compilerOptions->output,
            "mov [rbp %s %" PRIi32 "], rbx\n",
            sign,
            offsetValue
          );
          break;
        }
        case BASIC_TYPE_CHAR: {
          fprintf(
            compilerOptions->output,
            "mov [rbp %s %" PRIi32 "], rax\n",
            // I can use RAX without extending, because
            // Char is using EAX, and setting it will set
            // Upper bits of RAX to zero
            sign,
            offsetValue
          );
          break;
        }
        default: {
          ASSERT(false, "Unreachable");
        }
      }
      break;
    }
    case BASIC_TYPE_BOOL: {
      switch(typeFrom->basicType) {
        case BASIC_TYPE_INT:
        // Can use CHAR too, because setting EAX clears upper bits of RAx
        case BASIC_TYPE_CHAR: {
          fputs("xor ebx, ebx\n", compilerOptions->output);
          fputs("cmp rax, 0\n", compilerOptions->output);
          fputs("setne bl\n", compilerOptions->output);

          fprintf(
            compilerOptions->output,
            "mov [rbp %s %" PRIi32 "], bl\n",
            sign,
            offsetValue
          );
          break;
        }
        default: {
          ASSERT(false, "Unreachable");
        }
      }
      break;
    }
    case BASIC_TYPE_CHAR: {
      switch(typeFrom->basicType) {
        case BASIC_TYPE_INT: {
          fprintf(
            compilerOptions->output,
            "mov [rbp %s %" PRIi32 "], eax\n",
            sign,
            offsetValue
          );
          break;
        }
        case BASIC_TYPE_BOOL: {
          fputs("movzx ebx, al\n", compilerOptions->output);
          fprintf(
            compilerOptions->output,
            "mov [rbp %s %" PRIi32 "], ebx\n",
            sign,
            offsetValue
          );
          break;
        }
        default: {
          ASSERT(false, "Unreachable");
        }
      }
      break;
    }
    default: {
      ASSERT(false, "Unreachable!");
    }
  }
}

void generateAssignArrayValueToArrayVariableAsm(
  CompilerOptions *compilerOptions,
  Program *program,
  Type *elementType,
  size_t size,
  int32_t toStart,
  int32_t toEnd,
  TokenPriorityData *from,
  bool reverse
) {
  ASSERT(from->count == size, "TokenPriorityData's count is not equal to array size!");
  bool direction = toEnd > toStart;

  size_t elementSize = getTypeByteSize(elementType);
  if(elementSize >= 8) elementSize = 0;
  size_t elementOffset = getTypeByteOffset(elementType);

  for(size_t i = 0; i < size;i++) {
    Token *token = from->instructions[i];
    int32_t to = toStart + elementSize;

    switch(token->type) {
      case TOKEN_NAME: {
        int32_t offset = calculateOffset(program, token->data);
        generateAssignVariableToVariableAsm(
          compilerOptions,
          to,
          elementType,
          offset,
          elementType,
          offset < 0
        );
        break;
      }
      case TOKEN_VALUE: {
        generateAssignValueToVariableAsm(
          compilerOptions,
          program,
          to,
          elementType,
          token->data,
          reverse
        );
        break;
      }
      default: {
        ASSERT(false, "Token type not supproted!");
      }
    }

    toStart = direction ? toStart + elementOffset : toStart - elementOffset;
  }

  ASSERT(toStart == toEnd, "`to` offset not correct");
}

void generateAssignValueToVariableAsm(
  CompilerOptions *compilerOptions,
  Program *program,
  int32_t offsetTo,
  Type *typeTo,
  ValueData *valueData,
  bool reverse
) {
  Type *typeFrom = &valueData->type;
  ASSERT(canTypesConvert(typeFrom, typeTo), "Unreachable");

  const char *sign = getSign(offsetTo);
  int32_t offsetValue = abs(offsetTo);

  if(!isBasicType(typeFrom)) {
    if(typeFrom->basicType == BASIC_TYPE_ARRAY) {
      size_t arrayOffset = getTypeByteOffset(typeTo);
      ArrayType *arrayType = typeTo->data;

      int32_t offsetToEnd = offsetTo < 0 ? offsetTo + arrayOffset : offsetTo - arrayOffset;

      generateAssignArrayValueToArrayVariableAsm(
        compilerOptions,
        program,
        &arrayType->type,
        arrayType->numberOfElements,
        reverse ? offsetToEnd : offsetTo,
        reverse ? offsetTo : offsetToEnd,
        valueData->data,
        reverse
      );
      return;
    }
  }

  if(areTypesEqual(typeFrom, typeTo)) {
    switch (typeTo->basicType) {
      case BASIC_TYPE_INT: {
        fprintf(
          compilerOptions->output,
          "mov QWORD [rbp %s %" PRIi32 "], %" PRIu64 "\n",
          sign,
          offsetValue,
          getIntValue(valueData->data)
        );
        break;
      }
      case BASIC_TYPE_CHAR: {
        fprintf(
          compilerOptions->output,
          "mov DWORD [rbp %s %" PRIi32 "], %" PRIu32 "\n",
          sign,
          offsetValue,
          getCharValue(valueData->data)
        );
        break;
      }
      case BASIC_TYPE_BOOL: {
        fprintf(
          compilerOptions->output,
          "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
          sign,
          offsetValue,
          getNormalizedBoolValueFromUInt8(valueData->data)
        );
        break;
      }
      case BASIC_TYPE_FUNCTION: {
        FunctionTypeData *functionTypeData = valueData->data;
        fprintf(
          compilerOptions->output,
          "mov QWORD [rbp %s %" PRIi32 "], %s\n",
          sign,
          offsetValue,
          functionTypeData->name
        );
        break;
      }
      default: {
        ASSERT(false, "Type Not Supported!");
      }
    }
    return;
  }

  switch (typeTo->basicType) {
    case BASIC_TYPE_INT: {
      switch(valueData->type.basicType) {
        case BASIC_TYPE_BOOL: {
          fprintf(
            compilerOptions->output,
            "mov QWORD [rbp %s %" PRIi32 "], %" PRIu8 "\n",
            sign,
            offsetValue,
            getNormalizedBoolValueFromUInt8(valueData->data)
          );
          break;
        }
        case BASIC_TYPE_CHAR: {
          fprintf(
            compilerOptions->output,
            "mov QWORD [rbp %s %" PRIi32 "], %" PRIu32 "\n",
            sign,
            offsetValue,
            getCharValue(valueData->data)
          );
          break;
        }
        default: {
          ASSERT(false, "Type Not Supported!");
        }
      }
      break;
    }
    case BASIC_TYPE_CHAR: {
      switch(valueData->type.basicType) {
        case BASIC_TYPE_BOOL: {
          fprintf(
            compilerOptions->output,
            "mov DWORD [rbp %s %" PRIi32 "], %" PRIu8 "\n",
            sign,
            offsetValue,
            getNormalizedBoolValueFromUInt8(valueData->data)
          );
          break;
        }
        case BASIC_TYPE_INT: {
          fprintf(
            compilerOptions->output,
            "mov DWORD [rbp %s %" PRIi32 "], %" PRIu32 "\n",
            sign,
            offsetValue,
            getNormalizedCharValueFromUInt64(valueData->data)
          );
          break;
        }
        default: {
          ASSERT(false, "Type Not Supported!");
        }
      }
      break;
    }
    case BASIC_TYPE_BOOL: {
      switch(valueData->type.basicType) {
        case BASIC_TYPE_INT: {
          fprintf(
            compilerOptions->output,
            "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
            sign,
            offsetValue,
            getNormalizedBoolValueFromUInt64(valueData->data)
          );
          break;
        }
        case BASIC_TYPE_CHAR: {
          fprintf(
            compilerOptions->output,
            "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
            sign,
            offsetValue,
            getNormalizedBoolValueFromUInt32(valueData->data)
          );
          break;
        }
        default: {
          ASSERT(false, "Type Not Supported!");
        }
      }
      break;
    }
    default: {
      printf("typeTo->basicType: %d %s\n", typeTo->basicType, getBasicTypeName(typeTo->basicType));
      ASSERT(false, "Type Not Supported!");
    }
  }
}

void generateAssignTokenToVariableAsm(
  CompilerOptions *compilerOptions,
  Program *program,
  NameData *variable,
  Token *from
) {
  if(isOperationTokenType(from->type)) {
    generateBinaryOperationAsm(compilerOptions, program, from);

    int32_t offset = calculateOffset(program, variable);
    const char *sign = getSign(offset);
    int32_t offsetValue = abs(offset);
    fprintf(
      compilerOptions->output,
      "mov [rbp %s %" PRIi32 "], %s\n",
      sign,
      offsetValue,
      getRegisterBySize(REGISTER_A, variable->type)
    );

    return;
  }
  switch (from->type) {
    case TOKEN_NAME: {
      NameData *fromData = from->data;
      generateAssignVariableToVariableAsm(
        compilerOptions,
        calculateOffset(program, variable),
        variable->type,
        calculateOffset(program, fromData),
        fromData->type,
        false
      );
      break;
    }
    case TOKEN_INDEX: {
      generateIndexAsm(
        compilerOptions,
        program,
        from,
        REGISTER_A
      );

      int32_t offset = calculateOffset(program, variable);
      const char *sign = getSign(offset);
      int32_t offsetValue = abs(offset);
      fprintf(
        compilerOptions->output,
        "mov [rbp %s %" PRIi32 "], %s\n",
        sign,
        offsetValue,
        getRegisterBySize(REGISTER_A, variable->type)
      );
      break;
    }
    case TOKEN_VALUE: {
      generateAssignValueToVariableAsm(
        compilerOptions,
        program,
        calculateOffset(program, variable),
        variable->type,
        from->data,
        false
      );
      break;
    }
    case TOKEN_FUNCTION_CALL: {
      generateFunctionCallAsm(compilerOptions, program, from);

      int32_t offset = calculateOffset(program, variable);
      const char *sign = getSign(offset);
      int32_t offsetValue = abs(offset);
      fprintf(
        compilerOptions->output,
        "mov [rbp %s %" PRIi32 "], %s\n",
        sign,
        offsetValue,
        getRegisterBySize(REGISTER_A, variable->type)
      );
      break;
    }
    default: {
      printf("from->type: %d %s\n", from->type, getTokenTypeName(from->type));
      ASSERT(false, "Type not supported!");
      break;
    }
  }
}

// #endregion Assign

void generateProgramAsm(CompilerOptions *compilerOptions, Program *program) {
  if(program->functions && program->functions->size) {
    for(size_t i = 0; i < program->functions->capacity;i++) {
      const char *functionName = program->functions->elements[i].key;
      if(!functionName) continue;
      FunctionDefinition *data = program->functions->elements[i].value;
      generateFunctionAsm(compilerOptions, data);
    }
  }

  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];

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
          generateAssignTokenToVariableAsm(
            compilerOptions,
            program,
            data,
            program->instructions[++i]
          );
        } else {
          generateNameAsm(compilerOptions, program, token->data, REGISTER_A);
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
                "mov %s, [rbp %s %" PRIi32 "]\n",
                getRegisterBySize(REGISTER_DI, data->type),
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
  ASSERT(TOKEN_COUNT == 31, "Not all operations are implemented in generateAsm!");

  char *asmName = calloc(strlen(compilerOptions->basename) + 4 + 1, sizeof(char));
  sprintf(asmName, "%s.asm", compilerOptions->basename);
  FILE *out = openFile(asmName, "w");
  compilerOptions->output = out;
  if(!compilerOptions->silent) printf("[INFO]: Generating %s\n", asmName);

  prepareFileForCompile(out);
  generateProgramAsm(compilerOptions, compilerOptions->program);
  postCompile(compilerOptions);

  fclose(out);
  free(asmName);
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
