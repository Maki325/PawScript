#include "pawscript_error.h"
#include "includes.h"

const char *getPawscriptErrorName(PawscriptError error) {
  ASSERT(ERROR_COUNT == 2, "Not all errors are implemented in getPawscriptErrorName!");
  switch (error) {
    case ERROR_OPEN_FILE: return "Couldn't open file!";
    default:              return "Unknown Error!!!";
  }
}

void pawscript_exit(PawscriptError error) {
  printf("Exiting with code: \"%s\" (%d)\n", error, getPawscriptErrorName(error));
  exit(error);
}
