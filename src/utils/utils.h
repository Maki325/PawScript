#ifndef UTILS_H
#define UTILS_H
#include "../includes.h"
#include "../tokenizer.h"

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

FILE *openFile(const char *filePath, const char *modes);

#endif