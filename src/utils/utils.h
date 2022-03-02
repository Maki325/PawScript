#ifndef UTILS_H
#define UTILS_H
#include "../includes.h"
#include "../tokenizer.h"

typedef struct Args {
  size_t count;
  char **args;
} Args;

char *popArgument(Args *args);
char *getBasename(char *path);
char *getBasenameWithDirectory(char *path);
bool isDigit(char c);
int strnint(const char *str, size_t n);
void printProgram(Program *program);
void printToken(Token *token, size_t depth);
void printn(const char* string, size_t length);

int trimLeft(char **text, size_t *length);
int trimRight(const char *text, size_t *length);

int rstrncmp(const char *a, size_t aLength, const char *b, size_t bLength, size_t length);

#endif