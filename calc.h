#ifndef CALC_H
#define CALC_H

#include "string.h"
#include "stdlib.h"

#define NHASH 12289
#define PRIMENUMSIZE 26

/* Data type for links in the chain of symbols.      */
struct symrec
{
  char *name;  /* name of symbol                     */
  int type;    /* type of symbol: either VAR or FNCT */
  union {
    int var;           /* value of a VAR          */
    float floatVal;
    char* stringVal;
    int* pointer;
  } value;
  struct ast* ast;
  int scope;
  struct symrec *next;    /* link field              */
};

typedef struct symrec symrec;

#endif
