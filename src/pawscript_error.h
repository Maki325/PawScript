#ifndef PAWSCRIPT_ERROR
#define PAWSCRIPT_ERROR

typedef struct Token Token;

typedef enum PawscriptError {
  ERROR_OPEN_FILE = 1,
  ERROR_PARENTHESES_NOT_BALANCED,
  ERROR_BRACES_NOT_BALANCED,
  ERROR_BRACKETS_NOT_BALANCED,
  ERROR_IF_NO_CONDITION,
  ERROR_ELSE_AFTER_IF,
  ERROR_NAME_BEFORE_DECLARE_FUNCTION,
  ERROR_PARAMS_AFTER_DECLARE_FUNCTION,
  ERROR_TYPE_AFTER_PARAMS_FUNCTION,
  ERROR_BODY_AFTER_TYPE_FUNCTION,
  ERROR_NO_MUTABILITY_FOR_VARIABLE,
  ERROR_UNDECLARED_VARIABLE,
  ERROR_NO_TYPE_AFTER_ASSIGN_TYPE,
  ERROR_VARIABLE_NO_TYPE,
  ERROR_CANT_REASSIGN_VARIABLE_TYPE,
  ERROR_NO_ARGUMENT_AFTER_ASSIGN,
  ERROR_UNKNOWN_ARGUMENT_AFTER_ASSIGN,
  ERROR_UNINITIALIZED_VARIABLE,
  ERROR_OPERATION_DOESNT_HAVE_BOTH_OPERANDS,
  ERROR_OPERATION_NOT_ENOUGH_OPERANDS,
  ERROR_OPERAND_CANT_BE_USED,
  ERROR_OPERATION_NO_TYPE,
  ERROR_FUNCTION_CALL_ARGUMENTS_LENGTH_MISMATCH,
  ERROR_FUNCTION_CALL_ARGUMENTS_MISMATCH,
  ERROR_PLATFORM_NOT_SUPPORTED,
  ERROR_UNSUPPORTED_TYPE,
  ERROR_MULTIPLE_MAIN_FUNCTIONS,
  ERROR_NO_MAIN_FUNCTION,
  ERROR_UNKNOWN_TOKEN_IN_FUNCTION_CALL,
  ERROR_RETURN_TYPE_NOT_MATCHING,
  ERROR_NO_RETURN,
  ERROR_CANT_REASSIGN_CONST_VARIABLE,
  ERROR_EMPTY_CHAR,
  ERROR_TOO_MANY_CHARS,
  ERROR_INVALID_UNICODE_CHAR,
  ERROR_INVALID_BYTE_ESCAPE_SEQUENCE,
  ERROR_INVALID_UNICODE_ESCAPE_SEQUENCE,
  ERROR_UNKNOWN_ESCAPE_SEQUENCE,
  ERROR_UNKNOWN_TOKEN_IN_ARRAY,
  ERROR_ELEMENT_IN_ARRAY_TYPE_MISMATCH,
  ERROR_WRONG_ARRAY_TYPE,
  ERROR_EXPECTED_ONE_ELEMENT_IN_INDEXING_OPERATOR,
  ERROR_EXPECTED_NUMBER_VALUE_IN_INDEXING_OPERATOR,
  ERROR_INDEX_OUT_OF_BOUNDS,
  ERROR_WRONG_ARRAY_TYPE_SYNTAX,
  ERROR_WRONG_ARRAY_TYPE_SYNTAX_BOTH,
  ERROR_UNEXPECTED_TOKEN,
  ERROR_UNEXPECTED_FIRST_TOKEN_IN_PARAMETERS,
  ERROR_EXPECTED_NAME_TOKEN_IN_PARAMETERS,
  ERROR_EXPECTED_TOKEN_ASSIGN_TYPE_IN_PARAMETERS,
  ERROR_EXPECTED_TOKEN_TYPE_IN_PARAMETERS,
  ERROR_EXPECTED_TOKEN_COMMA_IN_PARAMETERS,
  ERROR_UNEXPECTED_TOKEN_IN_FUNCTION_ARGUMENT,
  ERROR_EXPECTED_TOKEN_COMMA_IN_FUNCTION_ARGUMENT,
  ERROR_TYPES_DONT_MATCH,
  ERROR_NO_TYPE_AFTER_AMPERSAND,
  ERROR_NO_TYPE_AFTER_AMPERSAND_MUT,
  ERROR_UNEXPECTED_TOKEN_AFTER_AMPERSAND_EXPECTED_TYPE,
  ERROR_NO_NAME_AFTER_AMPERSAND,
  ERROR_NO_NAME_AFTER_AMPERSAND_MUT,
  ERROR_UNEXPECTED_TOKEN_AFTER_AMPERSAND_EXPECTED_NAME,
  ERROR_COUNT
} PawscriptError;

const char *getPawscriptErrorName(PawscriptError error);

void exitError(PawscriptError pawscriptError);
void exitTokenError(PawscriptError pawscriptError, Token *token);

#endif // PAWSCRIPT_ERROR