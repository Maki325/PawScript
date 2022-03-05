#ifndef INCLUDES_H
#define INCLUDES_H

#define ASSERT(condition, message) assert(condition && message)

#ifdef PS_DEBUG
  #define PSLOG(...) printf(__VA_ARGS__)
#else
  #define PSLOG(...)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <ctype.h>
#include <math.h>
#include "utils/math.h"
#include "utils/bool.h"

#endif