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
  fputs("global _start\n", out);
  fputs("_start:\n", out);
}
void postCompile(FILE *out) {
  fputs("mov rax, 60\n", out);
  fputs("mov rdi, 0\n", out);
  fputs("syscall\n", out);
}

void generateFunctionAsm(CompilerOptions *compilerOptions, FunctionDefinition *functionData, int offset, HashTable *parentVariables) {
  // Optimize to use register for some fields instead of stack because faster 👍
  HashTable *variables = createHashTableFrom(parentVariables);

  TokenPriorityData *parameters = functionData->parameters + 16;
  for(size_t i = 0;i < parameters->count;i++) {
    if(parameters->instructions[i]->type != TOKEN_NAME) {
      continue;
    }
    NameData *nameData = parameters->instructions[i]->data;
    const char *name = nameData->name;
    if(existsElementInHashTable(variables, name)) {
      ASSERT(true, "Unreachable in `generateFunctionASM`");
    }
    CompileVariable *variable = createVariable(nameData, offset);
    setElementInHashTable(variables, name, variable);
    offset += getTypeByteSize(*nameData->type);
  }

  generateProgramAsm(compilerOptions, functionData->body, offset, variables, NULL);
}

int getVariableOffset(Program *program, CompileVariable *variable) {
  if(!existsElementInHashTable(program->variables, variable->nameData->name)) {
    if(!program->parent) return 0;
    int parentOffset = getVariableOffset(program->parent, variable);
    if(parentOffset == 0) {
      return 0;
    }
    return 8 + parentOffset;
  }
  NameData* getElementFromHashTable(program->variables, variable->nameData->name);
}

void generateProgramAsm(CompilerOptions *compilerOptions, Program *program, int offset, HashTable *parentVariables, HashTable *globalVariables) {
  HashTable *variables = createHashTableFrom(parentVariables);
  const char *name = NULL;

  (void) name;
  (void) compilerOptions;
  (void) offset;
  (void) variables;
  (void) globalVariables;

  printf("program->count: %zu\n", program->count);

  if(program->functions && program->functions->size) {
    for(size_t i = 0; i < program->functions->capacity;i++) {
      const char *functionName = program->functions->elements[i].key;
      if(!functionName) continue;
      FunctionDefinition *data = program->functions->elements[i].value;
      generateFunctionAsm(compilerOptions, data, offset, variables);
    }
  }

  for(size_t i = 0;i < program->count;i++) {
    Token *token = program->instructions[i];
    printf("generateProgramAsm: %s (%d)\n", getTokenTypeName(token->type), token->type);
    switch(token->type) {
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
