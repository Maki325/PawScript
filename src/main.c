#include "includes.h"
#include "utils/hashtable.h"
#include "utils/utils.h"
#include "tokenizer.h"
#include "compiler/compiler.h"
#include "interpreter/interpreter.h"

void usage(char *programName) {
  printf("Usage:\n");
  printf("  - %s int <file name>\n", programName);
  printf("  - %s com <file name> [-r] [-o <output name>]\n", programName);
}

int main(int argc, char *argv[]) {
  Args args = {argc, argv};
  char *programName = popArgument(&args);
  if(args.count == 0) {
    usage(programName);
    return 0;
  }
  char *mode = popArgument(&args);
  char error[512] = {0};
  if(strcmp(mode, "int") == 0) {
    if(args.count == 0) {
      usage(programName);
      return 0;
    }
    char *fileName = popArgument(&args);
    Program *program = createProgramFromFile(fileName, error);
    if(error[0] != 0) {
      printf("Error: %s\n", error);
      return 0;
    }
    interpret(program, error);
  } else if(strcmp(mode, "com") == 0) {
    if(args.count == 0) {
      usage(programName);
      return 0;
    }
    char *fileName = popArgument(&args);
    bool run = false;

    bool outputFileExists = false;
    char *outputFile = NULL;
    while(args.count != 0) {
      if(outputFileExists) {
        outputFile = popArgument(&args);
        outputFileExists = false;
        continue;
      }
      char *flag = popArgument(&args) + 1;
      if(strncmp(flag, "r", 1) == 0) {
        run = true;
      } else if(strncmp(flag, "o", 1) == 0) {
        outputFileExists = true;
      } else {
        printf("Unknown flag \"-%s\"\n", flag);
        usage(programName);
        return 0;
      }
    }
    if(outputFileExists) {
      printf("Please provide an output directory!\n");
      usage(programName);
      return 0;
    }
    
    char *basename = getBasenameWithDirectory(outputFile ? outputFile : fileName);
    char *asmName = calloc(strlen(basename) + 4 + 1, sizeof(char));
    sprintf(asmName, "%s.asm", basename);

    Program *program = createProgramFromFile(fileName, error);
    if(error[0] != 0) {
      printf("Error: %s\n", error);
      return 0;
    }

    FILE *out = fopen(asmName, "w");
    printf("[INFO]: Generating %s\n", asmName);
    generateAsm(program, out, error);
    fclose(out);

    compile(basename);

    if(run) {
      runProgram(basename);
    }
  } else {
    usage(programName);
    return 0;
  }
  if(error[0] != 0)
    printf("Error: %s\n", error);

  return 0;
}
