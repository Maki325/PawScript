#include "compiler.h"
#include "../utils/utils.h"

char *getInitializedType(Type type) {
  ASSERT(TYPES_COUNT == 3, "Not all types are implemented in getInitializedType!");
  static char* bytes[TYPES_COUNT] = {
    /* NONE */ "ERROR NONE!!!",
    /* INT  */ "dd",
    /* BOOL */ "dd"
  };
  if(type >= TYPES_COUNT) return "UI";
  return bytes[type];
}
char *getUninitializedType(Type type) {
  ASSERT(TYPES_COUNT == 3, "Not all types are implemented in getUninitializedType!");
  static char* bytes[TYPES_COUNT] = {
    /* NONE */ "ERROR NONE!!!",
    /* INT  */ "resd",
    /* BOOL */ "resd"
  };
  if(type >= TYPES_COUNT) return "UU";
  return bytes[type];
}
CompileVariable *createVariable(Type type, void *initialValue) {
  CompileVariable *value = malloc(sizeof(CompileVariable));
  value->type = type;
  value->usageCount = 0;
  value->initialValue = initialValue;

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
  fputs("global _start\n", out);
  fputs("_start:\n", out);
}
void postCompile(FILE *out) {
  fputs("mov rax, 60\n", out);
  fputs("mov rdi, 0\n", out);
  fputs("syscall\n", out);
}

bool generateBinaryOperationAsm(Token *token, HashTable *table, FILE *out, char *error) {
  ASSERT(TOKEN_COUNT == 20, "Not all operations are implemented in compile!");
  BinaryOperationValue *value = (BinaryOperationValue*) token->data;
  Token *leftToken = value->operandOne, *rightToken = value->operandTwo;
  if(!leftToken || !rightToken) {
    snprintf(
      error, 512,
      "%s:%zu:%zu: Add token doesn't have 2 params!",
      token->file, token->line, token->column
    );
    return false;
  }

  NameValue *nameValue = NULL;

  fprintf(out, "xor rax, rax\n");
  switch (leftToken->type) {
    case TOKEN_VALUE: {
      ValueData *value = leftToken->data;
      switch (value->type) {
        case TYPE_INT:  fprintf(out, "mov eax, %" PRIu32 "\n", *((uint32_t*) value->data)); break;
        case TYPE_BOOL: fprintf(out, "mov eax, %" PRIu8 "\n", *((uint8_t*) value->data));   break;
        case TYPE_FUNCTION:
        case TYPE_NONE:
        case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
      }
      break;
    }
    case TOKEN_NAME:
      nameValue = leftToken->data;
      switch (*nameValue->type) {
        case TYPE_INT:  fprintf(out, "mov eax, [%s]\n", nameValue->name); break;
        case TYPE_BOOL: fprintf(out, "mov ax, [%s]\n", nameValue->name);  break;
        case TYPE_FUNCTION:
        case TYPE_NONE:
        case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
      }
      break;
    case TOKEN_ADD:
    case TOKEN_SUBTRACT:
    case TOKEN_GREATER_THAN:
    case TOKEN_LESS_THAN:
    case TOKEN_EQUALS:
    case TOKEN_NOT_EQUALS: {
      fprintf(out, "push rbx\n");
      generateBinaryOperationAsm(leftToken, table, out, error);
      fprintf(out, "pop rbx\n");
      break;
    }
    case TOKEN_PRIORITY: {
      fprintf(out, "push rbx\n");
      TokenPriorityValue *value = leftToken->data;
      Program p = {.instructions = value->instructions, .count = value->count};
      generateProgramAsm(&p, table, out, error);
      fprintf(out, "pop rbx\n");
      break;
    }
    default: {
      fprintf(stderr, "Operand type left: %d, %s\n", leftToken->type, getTokenTypeName(leftToken->type));
      ASSERT(false, "Operand type not expected in generateBinaryOperationAsm!");
      break;
    }
  }

  fprintf(out, "xor rbx, rbx\n");
  switch (rightToken->type) {
    case TOKEN_VALUE: {
      ValueData *value = rightToken->data;
      switch (value->type) {
        case TYPE_INT:  fprintf(out, "mov ebx, %" PRIu32 "\n", *((uint32_t*) value->data)); break;
        case TYPE_BOOL: fprintf(out, "mov ebx, %" PRIu8 "\n", *((uint8_t*) value->data));   break;
        case TYPE_FUNCTION:
        case TYPE_NONE:
        case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
      }
      break;
    }
    case TOKEN_NAME:
      nameValue = rightToken->data;
      switch (*nameValue->type) {
        case TYPE_INT:  fprintf(out, "mov ebx, [%s]\n", nameValue->name); break;
        case TYPE_BOOL: fprintf(out, "mov bx, [%s]\n", nameValue->name);  break;
        case TYPE_FUNCTION:
        case TYPE_NONE:
        case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
      }
      break;
    case TOKEN_ADD:
    case TOKEN_SUBTRACT:
    case TOKEN_GREATER_THAN:
    case TOKEN_LESS_THAN:
    case TOKEN_EQUALS:
    case TOKEN_NOT_EQUALS: {
      fprintf(out, "push rax\n");
      generateBinaryOperationAsm(rightToken, table, out, error);
      fprintf(out, "mov ebx, eax\n");
      fprintf(out, "pop rax\n");
      break;
    }
    case TOKEN_PRIORITY: {
      fprintf(out, "push rax\n");
      TokenPriorityValue *value = rightToken->data;
      Program p = {.instructions = value->instructions, .count = value->count};
      generateProgramAsm(&p, table, out, error);
      fprintf(out, "mov ebx, eax\n");
      fprintf(out, "pop rax\n");
      break;
    }
    default: {
      fprintf(stderr, "Operand type right: %d, %s\n", leftToken->type, getTokenTypeName(leftToken->type));
      ASSERT(false, "Operand type not expected in generateBinaryOperationAsm!");
      break;
    }
  }

  // TODO: [IMPORTANT]: Do operations based on byte size (i.e. only ax, or eax, or rax)
  switch (token->type) {
  case TOKEN_ADD: {
    fprintf(out, "add eax, ebx\n");
    return true;
  }
  case TOKEN_SUBTRACT: {
    fprintf(out, "sub eax, ebx\n");
    return true;
  }
  case TOKEN_GREATER_THAN:
  case TOKEN_LESS_THAN:
  case TOKEN_EQUALS:
  case TOKEN_NOT_EQUALS: {
    fprintf(out, "; LG\n");
    fprintf(out, "push rcx\n");
    fprintf(out, "push rdx\n");

    // Prepare compare values
    fprintf(out, "mov ecx, 0\n");
    fprintf(out, "mov edx, 1\n");

    fprintf(out, "cmp eax, ebx\n");

    if(token->type == TOKEN_GREATER_THAN)
      fprintf(out, "cmovg ecx, edx\n");
    else if(token->type == TOKEN_LESS_THAN)
      fprintf(out, "cmovl ecx, edx\n");
    else if(token->type == TOKEN_EQUALS)
      fprintf(out, "cmove ecx, edx\n");
    else if(token->type == TOKEN_NOT_EQUALS)
      fprintf(out, "cmovne ecx, edx\n");

    fprintf(out, "mov eax, ecx\n");

    fprintf(out, "pop rdx\n");
    fprintf(out, "pop rcx\n");
    return true;
  }
  default:
    break;
  }

  return false;
}

void generateProgramAsm(Program *program, HashTable *table, FILE *out, char *error) {
  const char *name = NULL;
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    switch(token->type) {
      case TOKEN_NAME: {
        NameValue *value = token->data;
        const char *mName = value->name;
        if(!value->type) {
          snprintf(
            error, 512,
            "%s:%zu:%zu: No type for variable `%s`!",
            token->file, token->line, token->column,
            value->variableName
          );
          return;
        }
        if(i == 0) {
          name = mName;
          if(!existsElementInHashTable(table, name))
            setElementInHashTable(table, name, createVariable(*(value->type), NULL));

          if(i == program->count - 1 || program->instructions[i + 1]->type != TOKEN_ASSIGN) {
            fprintf(out, "xor rax, rax\n");
            CompileVariable *variable = getElementFromHashTable(table, name);
            switch (variable->type) {
              case TYPE_INT:  fprintf(out, "mov eax, [%s]\n", name); break;
              case TYPE_BOOL: fprintf(out, "mov ax, [%s]\n", name);  break;
              case TYPE_FUNCTION:
              case TYPE_NONE:
              case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
            }
          }
          break;
        }
        switch (program->instructions[i - 1]->type) {
          case TOKEN_ASSIGN: {
            if(strcmp(mName, name) == 0) {
              snprintf(
                error, 512,
                "%s:%zu:%zu: Can't assign a variable `%s` to itself!",
                token->file, token->line, token->column,
                value->variableName
              );
              return;
            } else if(!existsElementInHashTable(table, mName)) {
              snprintf(
                error, 512,
                "%s:%zu:%zu: Can't assign undeclared variable `%s`!",
                token->file, token->line, token->column,
                value->variableName
              );
              return;
            }
            fprintf(out, "; -- NAME ASSIGN \n");

            fprintf(out, "xor rax, rax\n");
            CompileVariable *variable = getElementFromHashTable(table, mName);
            switch (variable->type) {
              case TYPE_INT:  fprintf(out, "mov eax, [%s]\n", mName); break;
              case TYPE_BOOL: fprintf(out, "mov ax, [%s]\n", mName);  break;
              case TYPE_FUNCTION:
              case TYPE_NONE:
              case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
            }
            variable = getElementFromHashTable(table, name);
            switch (variable->type) {
              case TYPE_INT:  fprintf(out, "mov [%s], eax\n", name); break;
              case TYPE_BOOL: fprintf(out, "mov [%s], ax\n", name);  break;
              case TYPE_FUNCTION:
              case TYPE_NONE:
              case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
            }
            name = NULL;
            break;
          }
          default: {
            name = mName;
            if(!existsElementInHashTable(table, name))
              setElementInHashTable(table, name, createVariable(*(value->type), NULL));

            if(i == program->count - 1 || program->instructions[i + 1]->type != TOKEN_ASSIGN) {
              fprintf(out, "xor rax, rax\n");
              CompileVariable *variable = getElementFromHashTable(table, name);
              switch (variable->type) {
                case TYPE_INT:  fprintf(out, "mov eax, [%s]\n", name); break;
                case TYPE_BOOL: fprintf(out, "mov ax, [%s]\n", name);  break;
                case TYPE_FUNCTION:
                case TYPE_NONE:
                case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
              }
            }
            break;
          }
        }
        break;
      }
      case TOKEN_ASSIGN: {
        ASSERT(name != NULL, "Trying to assign to no variable!");
        CompileVariable *var = getElementFromHashTable(table, name);
        var->usageCount++;
        break;
      }
      case TOKEN_VALUE: {
        CompileVariable *value = getElementFromHashTable(table, name);
        if(value->initialValue || value->usageCount > 1) {
          fprintf(out, "xor rax, rax\n");
          ValueData *valueData = token->data;
          switch (valueData->type) {
            case TYPE_INT: fprintf(out, "mov eax, %" PRIu32 "\n", *((uint32_t*) valueData->data)); break;
            case TYPE_BOOL: fprintf(out, "mov ax, %" PRIu8 "\n", *((uint8_t*) valueData->data)); break;
            case TYPE_FUNCTION:
            case TYPE_NONE:
            case TYPES_COUNT: ASSERT(false, "Unreachable!"); break;
          }
          fprintf(out, "mov [%s], eax\n", name);
        } else {
          ValueData *valueData = token->data;
          value->initialValue = valueData->data;
        }
        name = NULL;
        break;
      }
      case TOKEN_PRINT: {
        const char *mName = token->data;
        CompileVariable *variable = getElementFromHashTable(table, mName);
        ASSERT(TYPES_COUNT == 3, "Not all types are implemented in compile print!");

        fprintf(out, "xor rax, rax\n");
        if(variable->type == TYPE_INT) {
          fprintf(out, "mov eax, [%s]\n", mName);
          fprintf(out, "mov rdi, rax\n");
          fprintf(out, "call print64\n");
        } else if(variable->type == TYPE_BOOL) {
          fprintf(out, "mov ax, [%s]\n", mName);
          fprintf(out, "mov rdi, rax\n");
          fprintf(out, "call print64\n");
        } 
        break;
      }
      case TOKEN_SEMICOLON: {
        name = NULL;
        break;
      }
      case TOKEN_ADD:
      case TOKEN_SUBTRACT:
      case TOKEN_GREATER_THAN:
      case TOKEN_LESS_THAN:
      case TOKEN_EQUALS:
      case TOKEN_NOT_EQUALS: {
        if(!generateBinaryOperationAsm(token, table, out, error)) {
          snprintf(
            error, 512,
            "%s:%zu:%zu: There's an error with ADD or SUBTRACT operation!",
            token->file, token->line, token->column
          );
          return;
        }
        
        if(i != 0 && program->instructions[i - 1]->type == TOKEN_ASSIGN) {
          fprintf(out, "mov [%s], eax\n", name);
        }
        break;
      }
      case TOKEN_PRIORITY: {
        TokenPriorityValue *value = (TokenPriorityValue*) token->data;
        Program prog = {.instructions = value->instructions, .count = value->count};
        generateProgramAsm(&prog, table, out, error);
        break;
      }
      case TOKEN_SCOPE: {
        Program *prog = (Program *) token->data;
        generateProgramAsm(prog, table, out, error);
        break;
      }
      case TOKEN_IF: {
        ControlFlowBlock *block = token->data;
        
        {
          Token *condition = block->condition;
          Program prog = {.instructions = &condition, .count = 1};
          generateProgramAsm(&prog, table, out, error);
        }

        Program *prog = block->program;

        fprintf(out, "cmp eax, 0\n");
        fprintf(out, "jne block_%zu_%zu\n", program->id, prog->id);
        fprintf(out, "jmp block_next_%zu_%zu\n", program->id, i + block->nextInstruction);
        goto compile_else_token;
      }
      case TOKEN_ELSE: compile_else_token: {
        ControlFlowBlock *block = token->data;

        Program *prog = block->program;

        fprintf(out, "block_%zu_%zu:\n", program->id, prog->id);
        generateProgramAsm(prog, table, out, error);
        fprintf(out, "jmp block_end_%zu_%zu\n", program->id, i + block->endInstruction);
        fprintf(out, "block_next_%zu_%zu:\n", program->id, i + 1);

        // TODO: Remove unnecessary `block_end` labels, by checking if it's an end block
        fprintf(out, "block_end_%zu_%zu:\n", program->id, i + 1);
        break;
      }
      default: {
        fprintf(stderr, "Error: Token(%s) not implemented in compilation!\n", getTokenTypeName(token->type));
        exit(1);
        break;
      }
    }
  }
}

void generateAsm(Program *program, const char *basename, bool silent, char *error) {
  ASSERT(TOKEN_COUNT == 20, "Not all operations are implemented in compile!");

  char *asmName = calloc(strlen(basename) + 4 + 1, sizeof(char));
  sprintf(asmName, "%s.asm", basename);
  FILE *out = openFile(asmName, "w");
  if(!silent) printf("[INFO]: Generating %s\n", asmName);

  prepareFileForCompile(out);
  HashTable *table = createHashTable(255);
  generateProgramAsm(program, table, out, error);
  postCompile(out);
  
  bool data = false, bss = false;
  for(size_t i = 0;i < table->capacity;i++) {
    if(table->elements[i].key == NULL) continue;
    const char* name = table->elements[i].key;
    CompileVariable *variable = table->elements[i].value;
    if(variable->initialValue == NULL) continue;
    if(!data) {
      fputs("\n", out);
      fputs("section .data\n", out);
      data = true;
    }
    fprintf(
      out, "%s %s %d\n",
      name,
      getInitializedType(variable->type),
      *((int*)variable->initialValue)
    );
  }
  for(size_t i = 0;i < table->capacity;i++) {
    if(table->elements[i].key == NULL) continue;
    const char* name = table->elements[i].key;
    CompileVariable *variable = table->elements[i].value;
    if(variable->initialValue != NULL) continue;
    if(!bss) {
      fputs("\n", out);
      fputs("section .bss\n", out);
      bss = true;
    }
    fprintf(
      out, "%s %s 1\n",
      name,
      getUninitializedType(variable->type)
    );
  }

  fclose(out);
}
void compile(const char *basename, bool silent) {
  // TODO: Check if folder exists for output
  // TODO: And create if it doesn't exist

  char call[128];
  snprintf(call, 128, "nasm -g -felf64 %s.asm", basename);
  if(!silent) printf("[CMD]: %s\n", call);
  system(call);

  snprintf(call, 128, "ld -o %s %s.o", basename, basename);
  if(!silent) printf("[CMD]: %s\n", call);
  system(call);
}
void runProgram(const char *basename, bool silent) {
  char call[128];
  snprintf(call, 128, "./%s", basename);
  if(!silent) printf("[CMD]: %s\n", call);
  system(call);
}
