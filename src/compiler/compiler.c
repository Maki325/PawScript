#include "compiler.h"
#include "../utils/utils.h"

char *getInitializedType(Type type) {
  ASSERT(TYPES_COUNT == 5, "Not all types are implemented in getInitializedType!");
  static char* bytes[TYPES_COUNT + 1] = {
    /* NONE     */ "ERROR NONE!!!",
    /* INT      */ "DQ",
    /* BOOL     */ "DQ",
    /* VOID     */ "ERROR VOID!!!",
    /* FUNCTION */ "ERROR FUNCTION!!!",
    /* COUNT    */ "ERROR COUNT!!!",
  };
  if(type > TYPES_COUNT) return "ERROR UNKNOWN!!!";
  return bytes[type];
}
char *getUninitializedType(Type type) {
  ASSERT(TYPES_COUNT == 5, "Not all types are implemented in getUninitializedType!");
  static char* bytes[TYPES_COUNT + 1] = {
    /* NONE     */ "ERROR NONE!!!",
    /* INT      */ "RESQ",
    /* BOOL     */ "RESQ",
    /* VOID     */ "ERROR VOID!!!",
    /* FUNCTION */ "ERROR FUNCTION!!!",
    /* COUNT    */ "ERROR COUNT!!!",
  };
  if(type > TYPES_COUNT) return "ERROR UNKNOWN!!!";
  return bytes[type];
}

CompileVariable *createVariable(NameData *nameData, int offset) {
  CompileVariable *value = malloc(sizeof(CompileVariable));
  value->nameData = nameData;
  value->type = *nameData->type;
  value->offset = offset;

  return value;
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
void prepareFileForCompile(FILE *out) {
  fputs("BITS 64\n", out);
  fputs("section .text\n", out);
  addPrintFunction(out);
}
void postCompile(FILE *out) {
  fputs("global _start\n", out);
  fputs("_start:\n", out);
  fputs("call main\n", out);
  // RDI is the return code of the program
  // Which is returned from the main function in the RAX register
  fputs("mov rdi, rax\n", out);
  // RAX is the syscall code for exiting the program
  fputs("mov rax, 60\n", out);
  fputs("syscall\n", out);
}

void generateFunctionAsm(CompilerOptions *compilerOptions, FunctionDefinition *functionData, int offset, HashTable *parentVariables) {
  // Optimize to use register for some fields instead of stack because faster 👍
  HashTable *variables = createHashTableFrom(parentVariables);
  fprintf(compilerOptions->output, "%s:\n", functionData->name);
  fputs("push rbp\n", compilerOptions->output);
  fputs("mov rbp, rsp\n", compilerOptions->output);
  fprintf(compilerOptions->output, "sub rsp, %" PRIi32 "\n", -functionData->body->variableOffset);

  generateProgramAsm(compilerOptions, functionData->body, offset, variables, NULL);

  fputs("mov rsp, rbp\n", compilerOptions->output);
  fputs("pop rbp\n", compilerOptions->output);
  fputs("ret\n", compilerOptions->output);
}

Token *nextToken(Program *program, size_t *i) {
  Token *token = program->instructions[(*i)++];
  return token;
}

void generateBinaryOperationAsm(CompilerOptions *compilerOptions, Token *operationToken) {
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
      switch(valueData->type) {
        case TYPE_INT: {
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
        case TYPE_BOOL: {
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
      NameData *nameData = left->data;
      switch(*nameData->type) {
        case TYPE_INT: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION LEFT NAME INT %s ---\n",
            nameData->variableName
          );
          fprintf(
            compilerOptions->output,
            "mov rax, [rbp %s %" PRIi32 "]\n",
            getSign(*nameData->offset),
            abs(*nameData->offset)
          );
          break;
        }
        case TYPE_BOOL: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION LEFT NAME BOOL %s ---\n",
            nameData->variableName
          );

          fprintf(
            compilerOptions->output,
            "movzx eax, BYTE [rbp %s %" PRIi32 "]\n",
            getSign(*nameData->offset),
            abs(*nameData->offset)
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
      generateBinaryOperationAsm(compilerOptions, left);
      break;
    }
  }

  switch (right->type) {
    case TOKEN_VALUE: {
      ValueData *valueData = right->data;
      switch(valueData->type) {
        case TYPE_INT: {
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
        case TYPE_BOOL: {
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
      NameData *nameData = right->data;
      switch(*nameData->type) {
        case TYPE_INT: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION RIGHT NAME INT %s ---\n",
            nameData->variableName
          );
          fprintf(
            compilerOptions->output,
            "mov rbx, [rbp %s %" PRIi32 "]\n",
            getSign(*nameData->offset),
            abs(*nameData->offset)
          );
          break;
        }
        case TYPE_BOOL: {
          fprintf(
            compilerOptions->output,
            "; --- BINARY OPERATION RIGHT NAME BOOL %s ---\n",
            nameData->variableName
          );

          fprintf(
            compilerOptions->output,
            "movzx ebx, BYTE [rbp %s %" PRIi32 "]\n",
            getSign(*nameData->offset),
            abs(*nameData->offset)
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
      generateBinaryOperationAsm(compilerOptions, right);
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

void generateAssignAsm(CompilerOptions *compilerOptions, NameData *data, Program *program, size_t *i) {
  Token *next = nextToken(program, i);

  if(isOperationTokenType(next->type)) {
    generateBinaryOperationAsm(compilerOptions, next);
    BinaryOperationData *binaryOperationData = next->data;
    Type operationType = binaryOperationData->type;

    switch(*data->type) {
      case TYPE_INT: {
        switch(operationType) {
          case TYPE_INT: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION INT -> INT %s ---\n",
              data->variableName
            );
            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rax\n",
              getSign(*data->offset),
              abs(*data->offset)
            );
            break;
          }
          case TYPE_BOOL: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION BOOL -> INT %s ---\n",
              data->variableName
            );

            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rax\n",
              getSign(*data->offset),
              abs(*data->offset)
            );
            break;
          }
          default: {
            ASSERT(false, "Type not supported!");
          }
        }
        break;
      }
      case TYPE_BOOL: {
        switch(operationType) {
          case TYPE_BOOL: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION BOOL -> BOOL %s ---\n",
              data->variableName
            );

            fputs("and rax, 1\n", compilerOptions->output);
            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rax\n",
              getSign(*data->offset),
              abs(*data->offset)
            );
            break;
          }
          case TYPE_INT: {
            fprintf(
              compilerOptions->output,
              "; --- ASSIGN OPERATION INT -> BOOL %s ---\n",
              data->variableName
            );
            fputs("and rax, 1\n", compilerOptions->output);
            fprintf(
              compilerOptions->output,
              "mov [rbp %s %" PRIi32 "], rax\n",
              getSign(*data->offset),
              abs(*data->offset)
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
    case TOKEN_VALUE: {
      ValueData *valueData = next->data;

      switch(*data->type) {
        case TYPE_INT: {
          switch(valueData->type) {
            case TYPE_INT: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN INT VALUE %" PRIu64 " -> INT %s ---\n",
                *((uint64_t*) valueData->data),
                data->variableName
              );

              fprintf(
                compilerOptions->output,
                "mov QWORD [rbp %s %" PRIi32 "], %" PRIu64 "\n",
                getSign(*data->offset),
                abs(*data->offset),
                *((uint64_t*) valueData->data)
              );
              break;
            }
            case TYPE_BOOL: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN BOOL VALUE %" PRIu8 " -> INT %s ---\n",
                *((uint8_t*) valueData->data),
                data->variableName
              );

              fprintf(
                compilerOptions->output,
                "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
                getSign(*data->offset),
                abs(*data->offset),
                *((uint8_t*) valueData->data)
              );
              break;
            }
            default: {
              ASSERT(false, "Type not supported!");
            }
          }
          break;
        }
        case TYPE_BOOL: {
          switch(valueData->type) {
            case TYPE_BOOL: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN BOOL VALUE %s -> BOOL %s ---\n",
                getBoolStringFromValue(valueData->data),
                data->variableName
              );

              fprintf(
                compilerOptions->output,
                "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
                getSign(*data->offset),
                abs(*data->offset),
                *((uint8_t*) valueData->data)
              );
              break;
            }
            case TYPE_INT: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN INT VALUE %" PRIu64 " (Normalized: %" PRIu8 ") -> BOOL %s ---\n",
                *((uint64_t*) valueData->data),
                getNormalizedBoolValueFromInt64(valueData->data),
                data->variableName
              );

              fprintf(
                compilerOptions->output,
                "mov BYTE [rbp %s %" PRIi32 "], %" PRIu8 "\n",
                getSign(*data->offset),
                abs(*data->offset),
                getNormalizedBoolValueFromInt64(valueData->data)
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
    case TOKEN_NAME: {
      NameData *nextData = next->data;

      switch(*data->type) {
        case TYPE_INT: {
          switch(*nextData->type) {
            case TYPE_INT: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN NAME INT %s -> INT %s ---\n",
                nextData->variableName, data->variableName
              );
              fprintf(
                compilerOptions->output,
                "mov rax, [rbp %s %" PRIi32 "]\n",
                getSign(*nextData->offset),
                abs(*nextData->offset)
              );
              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], rax\n",
                getSign(*data->offset),
                abs(*data->offset)
              );
              break;
            }
            case TYPE_BOOL: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN NAME BOOL %s -> INT %s ---\n",
                nextData->variableName, data->variableName
              );

              fprintf(
                compilerOptions->output,
                "movzx eax, BYTE [rbp %s %" PRIi32 "]\n",
                // We can use EAX, as it zeroes out the whole RAX for some reason
                getSign(*nextData->offset),
                abs(*nextData->offset)
              );
              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], rax\n",
                getSign(*data->offset),
                abs(*data->offset)
              );
              break;
            }
            default: {
              ASSERT(false, "Type not supported!");
            }
          }
          break;
        }
        case TYPE_BOOL: {
          switch(*nextData->type) {
            case TYPE_BOOL: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN NAME BOOL %s -> BOOL %s ---\n",
                nextData->variableName, data->variableName
              );

              fprintf(
                compilerOptions->output,
                "mov al, [rbp %s %" PRIi32 "]\n",
                getSign(*nextData->offset),
                abs(*nextData->offset)
              );
              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], al\n",
                getSign(*data->offset),
                abs(*data->offset)
              );
              break;
            }
            case TYPE_INT: {
              fprintf(
                compilerOptions->output,
                "; --- ASSIGN NAME INT %s -> BOOL %s ---\n",
                nextData->variableName, data->variableName
              );
              fprintf(
                compilerOptions->output,
                "mov rax, [rbp %s %" PRIi32 "]\n",
                getSign(*nextData->offset),
                abs(*nextData->offset)
              );
              fputs("and rax, 1\n", compilerOptions->output);
              fprintf(
                compilerOptions->output,
                "mov [rbp %s %" PRIi32 "], rax\n",
                getSign(*data->offset),
                abs(*data->offset)
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
      exit(-1);
      break;
    }
  }
}

NameData *getProgramVariable(Program *program, const char* name) {
  if(!existsElementInHashTable(program->variables, name)) {
    if(program->parent) {
      return getProgramVariable(program->parent, name);
    }
    return NULL;
  }
  return getElementFromHashTable(program->variables, name);
}

void generateProgramAsm(CompilerOptions *compilerOptions, Program *program, int offset, HashTable *parentVariables, HashTable *globalVariables) {
  HashTable *variables = createHashTableFrom(parentVariables);
  const char *name = NULL;
  (void) name;

  if(program->functions && program->functions->size) {
    for(size_t i = 0; i < program->functions->capacity;i++) {
      const char *functionName = program->functions->elements[i].key;
      if(!functionName) continue;
      FunctionDefinition *data = program->functions->elements[i].value;
      generateFunctionAsm(compilerOptions, data, offset, variables);
    }
  }

  for(size_t i = 0;i < program->count;i++) {
    Token *token = nextToken(program, &i);
    switch(token->type) {
      case TOKEN_NAME: {
        NameData *data = token->data;
        fprintf(
          compilerOptions->output,
          "; --- TOKEN NAME %s ---\n",
          data->variableName
        );

        Token *next = nextToken(program, &i);
        if(next->type == TOKEN_ASSIGN) {
          generateAssignAsm(compilerOptions, data, program, &i);
        } else {
          i--;
        }
        break;
      }
      case TOKEN_VALUE: {
        ValueData *data = token->data;
        switch(data->type) {
          case TYPE_BOOL: {
            fprintf(
              compilerOptions->output,
              "; --- TOKEN VALUE BOOL %" PRIu8 " ---\n",
              *((uint8_t*) data->data)
            );

            fprintf(
              compilerOptions->output,
              "mov eax, %" PRIu8 "\n",
              // Setting EAX clears RAX
              *((uint8_t*) data->data)
            );
            break;
          }
          case TYPE_INT: {
            fprintf(
              compilerOptions->output,
              "; --- TOKEN VALUE INT %" PRIu64 " ---\n",
              *((uint64_t*) data->data)
            );

            fprintf(
              compilerOptions->output,
              "mov rax, %" PRIu64 "\n",
              *((uint64_t*) data->data)
            );
            break;
          }
          default: {
            ASSERT(false, "Type not supported!");
          }
        }
        break;
      }
      case TOKEN_PRINT: {
        NameData *data = getProgramVariable(program, token->data);
        ASSERT(data, "Unreachable!");
        fprintf(
          compilerOptions->output,
          "; --- TOKEN PRINT %s ---\n",
          data->variableName
        );

        fprintf(
          compilerOptions->output,
          "mov rdi, [rbp %s %" PRIi32 "]\n",
          getSign(*data->offset),
          abs(*data->offset)
        );
        fputs("call print64\n", compilerOptions->output);

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
            .variables = NULL,
          };
          generateProgramAsm(compilerOptions, &p, offset, parentVariables, globalVariables);
        } else {
          ASSERT(false, "Not implemented yet");
        }
        
        fputs("mov rsp, rbp\n", compilerOptions->output);
        fputs("pop rbp\n", compilerOptions->output);
        fputs("ret\n", compilerOptions->output);
        break;
      }
      case TOKEN_SEMICOLON: {
        // What should it do?
        break;
      }
      default: {
        fprintf(stderr, "Error: Token type `%s` not implemented in compilation!\n", getTokenTypeName(token->type));
        exit(1);
        break;
      }
    }
  }

  free(variables);
}

void generateAsm(CompilerOptions *compilerOptions) {
  ASSERT(TOKEN_COUNT == 29, "Not all operations are implemented in compile!");

  char *asmName = calloc(strlen(compilerOptions->basename) + 4 + 1, sizeof(char));
  sprintf(asmName, "%s.asm", compilerOptions->basename);
  FILE *out = openFile(asmName, "w");
  compilerOptions->output = out;
  if(!compilerOptions->silent) printf("[INFO]: Generating %s\n", asmName);

  prepareFileForCompile(out);
  HashTable *globalVariables = createHashTable(256);

  generateProgramAsm(compilerOptions, compilerOptions->program, 0, NULL, globalVariables);
  postCompile(out);

  bool data = false, bss = false;

  (void) data;
  (void) bss;

  for(size_t i = 0;i < globalVariables->capacity;i++) {
    if(globalVariables->elements[i].key == NULL) continue;
    const char* name = globalVariables->elements[i].key;

    (void) name;
    // CompileVariable *variable = table->elements[i].value;
    // if(variable->initialValue == NULL) continue;
    // if(!data) {
    //   fputs("\n", out);
    //   fputs("section .data\n", out);
    //   data = true;
    // }
    // fprintf(
    //   out, "%s %s %d\n",
    //   name,
    //   getInitializedType(variable->type),
    //   *((int*)variable->initialValue)
    // );
  }
  for(size_t i = 0;i < globalVariables->capacity;i++) {
    if(globalVariables->elements[i].key == NULL) continue;
    const char* name = globalVariables->elements[i].key;

    (void) name;
    // CompileVariable *variable = table->elements[i].value;
    // if(variable->initialValue != NULL) continue;
    // if(!bss) {
    //   fputs("\n", out);
    //   fputs("section .bss\n", out);
    //   bss = true;
    // }
    // fprintf(
    //   out, "%s %s 1\n",
    //   name,
    //   getUninitializedType(variable->type)
    // );
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
