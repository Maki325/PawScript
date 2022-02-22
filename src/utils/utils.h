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
void printToken(Token *token);
void printn(const char* string, size_t length);

int trimLeft(char **text, size_t *length);

#endif