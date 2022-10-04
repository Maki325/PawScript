#ifndef UTILS_H
#define UTILS_H
#include "../includes.h"
#include "../tokenizer/tokenizer.h"

#define TAB_SPACES 2

typedef struct Args {
  size_t count;
  char **args;
} Args;

char *popArgument(Args *args);
char *getBasename(char *path);
char *getBasenameWithDirectory(char *path);
bool isDigit(char c);
void printProgram(Program *program, unsigned int depth);
void printToken(Token *token, unsigned int depth, size_t index);
void printn(const char* string, size_t length);

int trimLeft(char **text, size_t *length);
int trimRight(const char *text, size_t *length);

int rstrncmp(const char *a, size_t aLength, const char *b, size_t bLength, size_t length);

void printTokenLocation(Token *token, FILE *out);

int strnint(const char *str, size_t n);
uint32_t strnuint32(const char *str, size_t n);
uint64_t strnuint64(const char *str, size_t n);

const char *getBoolStringFromValue(uint8_t *value);
uint8_t getNormalizedBoolValueFromUInt64(uint64_t *value);
uint8_t getNormalizedBoolValueFromUInt32(uint32_t *value);
uint8_t getNormalizedBoolValueFromUInt8(uint8_t *value);

uint32_t getNormalizedCharValueFromUInt64(uint64_t *value);

const char *getSign(int32_t value);

uint64_t getIntValue(void *data);
uint32_t getCharValue(void *data);
uint8_t getBoolValue(void *data);

const char *getFunctionNameFromCall(FunctionCallData *data);

Type getFunctionReturnTypeFromCall(FunctionCallData *data);

const char *getBasicTypeName(BasicType type);
const char *getTypeName(Type type);

extern const uint8_t CONT_MASK;

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4 bits
/// for width 3, and 3 bits for width 4.
inline static uint32_t utf8_first_byte(uint8_t byte, uint32_t width) {
  return (byte & (0x7F >> width));
}
/// Returns the value of `ch` updated with continuation byte `byte`.
inline static uint32_t utf8_acc_cont_byte(uint32_t ch, uint8_t byte) {
  return (ch << 6) | (byte & CONT_MASK);
}

// Line by line replica of `next_code_point` from the rust compiler
// Path: library\core\src\str\validations.rs
uint32_t turnCharsIntoCodePoint(const char *chars, size_t *length);

FILE *openFile(const char *filePath, const char *modes);

#endif