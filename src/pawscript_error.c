#include "pawscript_error.h"
#include "includes.h"
#include "tokenizer.h"

const char *getPawscriptErrorName(PawscriptError error) {
  ASSERT(ERROR_COUNT == 23, "Not all errors are implemented in getPawscriptErrorName!");
  switch (error) {
    case ERROR_OPEN_FILE:                               return "Couldn't open file!";
    case ERROR_PARENTHESES_NOT_BALANCED:                return "Parentheses are not balanced!";
    case ERROR_BRACES_NOT_BALANCED:                     return "Braces are not balanced!";
    case ERROR_IF_NO_CONDITION:                         return "`if` needs to have a condition!";
    case ERROR_ELSE_AFTER_IF:                           return "`else` must be preceded by `if`!";
    case ERROR_NAME_BEFORE_DECLARE_FUNCTION:            return "`=>` must be preceded by the name of the function!";
    case ERROR_PARAMS_AFTER_DECLARE_FUNCTION:           return "`=>` must be followed by the parameters of the function!";
    case ERROR_TYPE_AFTER_PARAMS_FUNCTION:              return "Function parameters must be followed by either the function body or the return type!";
    case ERROR_BODY_AFTER_TYPE_FUNCTION:                return "Function return type must be followed by the function body!";
    case ERROR_NO_MUTABILITY_FOR_VARIABLE:              return "A variable name must be preceded by either `const` or `mut`!";
    case ERROR_UNDECLARED_VARIABLE:                     return "Cant find variable declaration!";
    case ERROR_NO_TYPE_AFTER_ASSIGN_TYPE:               return "A type is needed after the assign type(`:`) symbol!";
    case ERROR_VARIABLE_NO_TYPE:                        return "The variable doesn't have a type!";
    case ERROR_CANT_REASSIGN_VARIABLE_TYPE:             return "Can't reassign type of the variable!";
    case ERROR_NO_ARGUMENT_AFTER_ASSIGN:                return "An argument is needed after the assign(`=`) symbol!";
    case ERROR_UNINITIALIZED_VARIABLE:                  return "The variable must be initialized!";
    case ERROR_OPERATION_DOESNT_HAVE_BOTH_OPERANDS:     return "Operation doesn't have all the operands!";
    case ERROR_OPERATION_NOT_ENOUGH_OPERANDS:           return "Not enought operands for operation!";
    case ERROR_OPERAND_CANT_BE_USED:                    return "Operand can't be used!";
    case ERROR_OPERATION_NO_TYPE:                       return "The oepration doesn't have a type!";
    case ERROR_FUNCTION_CALL_ARGUMENTS_LENGTH_MISMATCH: return "Not the correct amount of arguments!";
    case ERROR_FUNCTION_CALL_ARGUMENTS_MISMATCH:        return "Argument type doesn't match!";
    default:                                            return "Unknown Error!!!";
  }
}

void pawscript_exit(PawscriptError error) {
  printf("Exiting with error: \"%s\" (%d)\n", getPawscriptErrorName(error), error);
  exit(error);
}

void exitError(PawscriptError pawscriptError, Token *token) {
  fprintf(stderr, "ERROR: %s at: %s:%zu:%zu\n",
    getPawscriptErrorName(pawscriptError),
    token->file, token->line, token->column);
  exit(pawscriptError);
}
