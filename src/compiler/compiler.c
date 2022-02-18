#include "compiler.h"

char *getInitializedType(Type type) {
  static char* bytes[TYPES_COUNT] = {
    /* INT */ "dd"
  };
  if(type >= TYPES_COUNT) return "UI";
  return bytes[type];
}
char *getUninitializedType(Type type) {
  static char* bytes[TYPES_COUNT] = {
    /* INT */ "resd"
  };
  if(type >= TYPES_COUNT) return "UU";
  return bytes[type];
}
CompileVariable *createVariable(Type type, void *initialValue) {
  CompileVariable *value = malloc(sizeof(CompileVariable));
  value->type = type;
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

bool generateBinaryOperationAsm(Token *token, FILE *out, const char *error) {
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

  fprintf(out, "mov eax, %" PRIu32 "\n", *((uint32_t*) leftToken->data));
  switch (leftToken->type) {
  case TOKEN_VALUE:
    fprintf(out, "mov eax, %" PRIu32 "\n", *((uint32_t*) leftToken->data));
    break;
  case TOKEN_NAME:
    fprintf(out, "mov eax, [%s]\n", (const char*) leftToken->data);
    break;
  default:
    fprintf(out, "push rbx\n");
    generateBinaryOperationAsm(leftToken, out, error);
    fprintf(out, "pop rbx\n");
    break;
  }

  switch (rightToken->type) {
  case TOKEN_VALUE:
    fprintf(out, "mov ebx, %" PRIu32 "\n", *((uint32_t*) rightToken->data));
    break;
  case TOKEN_NAME:
    fprintf(out, "mov ebx, [%s]\n", (const char*) rightToken->data);
    break;
  default:
    fprintf(out, "push rax\n");
    generateBinaryOperationAsm(rightToken, out, error);
    fprintf(out, "pop rax\n");
    break;
  }

  uint32_t *sum = malloc(sizeof(uint32_t));

  switch (token->type) {
  case TOKEN_ADD: {
    fprintf(out, "add eax, ebx\n");
    return true;
  }
  case TOKEN_SUBTRACT: {
    fprintf(out, "sub eax, ebx\n");
    return true;
  }
  default:
    break;
  }

  return false;
}

void generateAsm(Program *program, FILE *out, const char *error) {
  ASSERT(TOKEN_COUNT == 8, "Not all operations are implemented in compile!");
  prepareFileForCompile(out);
  char *name = NULL;
  Type type = TYPES_COUNT;
  HashTable *table = createHashTable(255);
  for(int i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    switch(token->type) {
      case TOKEN_TYPE: {
        type = (Type) token->data;
        break;
      }
      case TOKEN_NAME: {
        char *mName = token->data;
        if(i == 0) {
          snprintf(
            error, 512,
            "%s:%zu:%zu: No type for variable `%s`!",
            token->file, token->line, token->column,
            mName
          );
          return;
        }
        switch (program->instructions[i - 1]->type) {
        case TOKEN_ASSIGN: {
          if(strcmp(mName, name) == 0) {
            snprintf(
              error, 512,
              "%s:%zu:%zu: Can't assign a variable `%s` to itself!",
              token->file, token->line, token->column,
              mName
            );
            return;
          } else if(!existsElementInHashTable(table, mName)) {
            snprintf(
              error, 512,
              "%s:%zu:%zu: Can't assign undeclared variable `%s`!",
              token->file, token->line, token->column,
              mName
            );
            return;
          }
          fprintf(out, "; -- NAME ASSIGN \n");
          fprintf(out, "mov eax, [%s]\n", mName);
          fprintf(out, "mov [%s], eax\n", name);
          name = NULL;
          break;
        }
        case TOKEN_TYPE: {
          name = mName;
          setElementInHashTable(table, name, createVariable(type, NULL));
          type = TYPES_COUNT;
          break;
        }
        default:
          break;
        }
        break;
      }
      case TOKEN_ASSIGN: {
        ASSERT(name != NULL, "Trying to assign to no variable!");
        break;
      }
      case TOKEN_VALUE: {
        CompileVariable *value = getElementFromHashTable(table, name);
        value->initialValue = token->data;
        name = NULL;
        break;
      }
      case TOKEN_PRINT: {
        char *mName = token->data;
        CompileVariable *variable = getElementFromHashTable(table, mName);
        ASSERT(TYPES_COUNT == 1, "Not all types are implemented in compile print!");
        
        if(variable->type == TYPE_INT) {
          fprintf(out, "mov eax, [%s]\n", mName);
          fprintf(out, "mov rdi, rax\n");
          fprintf(out, "call print64\n");
        }
        break;
      }
      case TOKEN_SEMICOLON: {
        type = TYPES_COUNT;
        name = NULL;
        break;
      }
      case TOKEN_ADD:
      case TOKEN_SUBTRACT: {
        if(!generateBinaryOperationAsm(token, out, error)) {
          snprintf(
            error, 512,
            "%s:%zu:%zu: There's an error with ADD or SUBTRACT operation!",
            token->file, token->line, token->column
          );
          return;
        }
        
        fprintf(out, "mov [%s], eax\n", name);
        break;
      }
    }
  }
  postCompile(out);
  
  bool data = false, bss = false;
  for(int i = 0;i < table->capacity;i++) {
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
  for(int i = 0;i < table->capacity;i++) {
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
}
void compile(const char *basename) {
  // TODO: Check if folder exists for output
  // TODO: And create if it doesn't exist

  char call[128];
  snprintf(call, 128, "nasm -felf64 %s.asm", basename);
  printf("[CMD]: %s\n", call);
  system(call);

  snprintf(call, 128, "ld -o %s %s.o", basename, basename);
  printf("[CMD]: %s\n", call);
  system(call);
}
void runProgram(const char *basename) {
  char call[128];
  snprintf(call, 128, "./%s", basename);
  printf("[CMD]: %s\n", call);
  system(call);
}
