#include "pawscript_error.h"
#include "includes.h"
#include "tokenizer/tokenizer.h"

const char *getPawscriptErrorName(PawscriptError error) {
  ASSERT(ERROR_COUNT == 42, "Not all errors are implemented in getPawscriptErrorName!");
  switch (error) {
    case ERROR_OPEN_FILE:                                  return "Couldn't open file!";
    case ERROR_PARENTHESES_NOT_BALANCED:                   return "Parentheses are not balanced!";
    case ERROR_BRACES_NOT_BALANCED:                        return "Braces are not balanced!";
    case ERROR_BRACKETS_NOT_BALANCED:                      return "Brackets are not balanced!";
    case ERROR_IF_NO_CONDITION:                            return "`if` needs to have a condition!";
    case ERROR_ELSE_AFTER_IF:                              return "`else` must be preceded by `if`!";
    case ERROR_NAME_BEFORE_DECLARE_FUNCTION:               return "`=>` must be preceded by the name of the function!";
    case ERROR_PARAMS_AFTER_DECLARE_FUNCTION:              return "`=>` must be followed by the parameters of the function!";
    case ERROR_TYPE_AFTER_PARAMS_FUNCTION:                 return "Function parameters must be followed by either the function body or the return type!";
    case ERROR_BODY_AFTER_TYPE_FUNCTION:                   return "Function return type must be followed by the function body!";
    case ERROR_NO_MUTABILITY_FOR_VARIABLE:                 return "A variable name must be preceded by either `const` or `mut`!";
    case ERROR_UNDECLARED_VARIABLE:                        return "Cant find variable declaration!";
    case ERROR_NO_TYPE_AFTER_ASSIGN_TYPE:                  return "A type is needed after the assign type(`:`) symbol!";
    case ERROR_VARIABLE_NO_TYPE:                           return "The variable doesn't have a type!";
    case ERROR_CANT_REASSIGN_VARIABLE_TYPE:                return "Can't reassign type of the variable!";
    case ERROR_NO_ARGUMENT_AFTER_ASSIGN:                   return "An argument is needed after the assign(`=`) symbol!";
    case ERROR_UNKNOWN_ARGUMENT_AFTER_ASSIGN:              return "An unknown argument is set after the assign(`=`) symbol!";
    case ERROR_UNINITIALIZED_VARIABLE:                     return "The variable must be initialized!";
    case ERROR_OPERATION_DOESNT_HAVE_BOTH_OPERANDS:        return "Operation doesn't have all the operands!";
    case ERROR_OPERATION_NOT_ENOUGH_OPERANDS:              return "Not enought operands for operation!";
    case ERROR_OPERAND_CANT_BE_USED:                       return "Operand can't be used!";
    case ERROR_OPERATION_NO_TYPE:                          return "The oepration doesn't have a type!";
    case ERROR_FUNCTION_CALL_ARGUMENTS_LENGTH_MISMATCH:    return "Not the correct amount of arguments!";
    case ERROR_FUNCTION_CALL_ARGUMENTS_MISMATCH:           return "Argument type doesn't match!";
    case ERROR_PLATFORM_NOT_SUPPORTED:                     return "Platform not supported!";
    case ERROR_UNSUPPORTED_TYPE:                           return "Type not supported!";
    case ERROR_MULTIPLE_MAIN_FUNCTIONS:                    return "There can only be one `main` function!";
    case ERROR_NO_MAIN_FUNCTION:                           return "There must be a `main` function!";
    case ERROR_UNKNOWN_TOKEN_IN_FUNCTION_CALL:             return "Unknown token type in function call!";
    case ERROR_RETURN_TYPE_NOT_MATCHING:                   return "Return type not matching!";
    case ERROR_NO_RETURN:                                  return "No return in function!";
    case ERROR_CANT_REASSIGN_CONST_VARIABLE:               return "Can't reassign const variable!";
    case ERROR_EMPTY_CHAR:                                 return "Empty chars are not allowed!";
    case ERROR_TOO_MANY_CHARS:                             return "Can't have multiple chars in one!";
    case ERROR_UNKNOWN_TOKEN_IN_ARRAY:                     return "Unknown token in array!";
    case ERROR_ELEMENT_IN_ARRAY_TYPE_MISMATCH:             return "Element in array doesn't have the correct type!";
    case ERROR_WRONG_ARRAY_TYPE:                           return "Variable and the array are not the same type!";
    case ERROR_EXPECTED_ONE_ELEMENT_IN_INDEXING_OPERATOR:  return "Indexing operator can have only one element!";
    case ERROR_EXPECTED_NUMBER_VALUE_IN_INDEXING_OPERATOR: return "Expected number in indexing operator!";
    case ERROR_INDEX_OUT_OF_BOUNDS:                        return "Index out is out of bounds of the array!";
    case ERROR_WRONG_ARRAY_TYPE_SYNTAX:                    return "Wrong array type syntax! Expected `[<type>; <size>]` or `[<type>]`!";
    default:                                               return "Unknown Error!!!";
  }
}

void exitTokenError(PawscriptError pawscriptError, Token *token) {
  fprintf(stderr, "ERROR: %s at: %s:%zu:%zu\n",
    getPawscriptErrorName(pawscriptError),
    token->file, token->line, token->column);
  exit(pawscriptError);
}

void exitError(PawscriptError pawscriptError) {
  fprintf(stderr, "ERROR: %s (%d)\n",
    getPawscriptErrorName(pawscriptError), pawscriptError);
  exit(pawscriptError);
}
