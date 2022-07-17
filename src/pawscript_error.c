#include "pawscript_error.h"
#include "includes.h"

const char *getPawscriptErrorName(PawscriptError error) {
  ASSERT(ERROR_COUNT == 2, "Not all errors are implemented in getPawscriptErrorName!");
  switch (error) {
    case ERROR_OPEN_FILE:                     return "Couldn't open file!";
    case ERROR_PARENTHESES_NOT_BALANCED:      return "Parentheses are not balanced!";
    case ERROR_BRACES_NOT_BALANCED:           return "Braces are not balanced!";
    case ERROR_IF_NO_CONDITION:               return "`if` needs to have a condition!";
    case ERROR_ELSE_AFTER_IF:                 return "`else` must be preceded by `if`!";
    case ERROR_NAME_BEFORE_DECLARE_FUNCTION:  return "`=>` must be preceded by the name of the function!";
    case ERROR_PARAMS_AFTER_DECLARE_FUNCTION: return "`=>` must be followed by the parameters of the function!";
    case ERROR_TYPE_AFTER_PARAMS_FUNCTION:    return "Function parameters must be followed by either the function body or the return type!";
    case ERROR_BODY_AFTER_TYPE_FUNCTION:      return "Function return type must be followed by the function body!";
    default:                                  return "Unknown Error!!!";
  }
}

void pawscript_exit(PawscriptError error) {
  printf("Exiting with error: \"%s\" (%d)\n", getPawscriptErrorName(error), error);
  exit(error);
}
