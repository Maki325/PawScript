#ifndef PAWSCRIPT_ERROR
#define PAWSCRIPT_ERROR

typedef enum PawscriptError {
  ERROR_OPEN_FILE = 1,
  ERROR_PARENTHESES_NOT_BALANCED,
  ERROR_BRACES_NOT_BALANCED,
  ERROR_IF_NO_CONDITION,
  ERROR_ELSE_AFTER_IF,
  ERROR_COUNT
} PawscriptError;

const char *getPawscriptErrorName(PawscriptError error);

void pawscriptExit(PawscriptError error);

#endif // PAWSCRIPT_ERROR