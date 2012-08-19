#ifndef _GLOBAL_H
#define _GLOBAL_H

int linenum;
char filename[50];

struct symrec ***all_sym;
int curSize;
int curScope;
int offset;
int localoffset;
int paramoffset;
char* functionString;
char* declarationString;
char* arrayString;

FILE* myfile;
FILE* tempfile;
FILE* sfile;
FILE* commfile;
int tempNum;
int fncNum;
int blockNum;

struct stack {
	char *type;
	struct stack* next;
} *stack;

#endif
