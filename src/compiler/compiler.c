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

void generateProgramAsm(Program *program, HashTable *table, FILE *out, char *error) {
  const char *name = NULL;
  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    switch(token->type) {
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
  for(size_t i = 0;i < table->capacity;i++) {
    if(table->elements[i].key == NULL) continue;
    const char* name = table->elements[i].key;
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
