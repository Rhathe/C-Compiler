%{
#include <stdio.h>
#include <string.h>
#include "grammar.tab.h"
#include "global.h"
#include "calc.h"

struct yylvalStruct {
	int intVal;
	float floatVal;
	unsigned char* stringVal;
} yylval2;

extern YYSTYPE yylval;
extern int yyerror;

int strSize;
unsigned char buffer[256];
char myUnsign[256];
char myType[256];
char mySize[256];

#ifndef NHASH
#define NHASH 12289
#endif

unsigned int symhash2(char *sym) {
	unsigned int hash = 0;
	int c;
	
	while (c = *sym++) 
		hash = hash*9 ^c;
	
	return hash%NHASH;
}

struct symrec *lookup2(struct symrec* sym_table, char* sym) {
	unsigned int i = symhash(sym);
	int scount = NHASH;
	
	while (--scount >= 0) {
		if (sym_table[i].name && !strcmp(sym_table[i].name,sym))
			return (sym_table + i);
		if (!sym_table[i].name) {
			return NULL;
		}
		
		if (i++ >= NHASH)
			i = 0;
	}
	fputs("symbol table overflow\n", stderr);
	abort();
}

symrec* findsym2(char* b, int namespace) { 
	int i; symrec* temp;
	for(i = curScope; i >=0; --i) {
		temp = lookup2(all_sym[i][namespace], b); 
		if (temp != NULL) break;
	}
	
	return temp;
}

%}

%x STRINGSC
%x ESCAPE
%x NEXTESCAPE
%x FIRSTCHAR
%x NEXTCHAR
%x CHAROVERFLOW

%%

<INITIAL>(L\")|(\")			{BEGIN(STRINGSC); memset(buffer, 0, 256); strSize = 0;}
<INITIAL>(L\')|(\')			{BEGIN(FIRSTCHAR); memset(buffer, 0, 256); strSize = 0;}
<STRINGSC>\"				{BEGIN(INITIAL); yylval.stringVal = buffer; return STRING;}
<STRINGSC>\n				{fprintf(stderr, "%s:%d:forgot quote\n", filename, linenum); BEGIN(INITIAL);}
<STRINGSC>\\				{BEGIN(ESCAPE); buffer[strSize] = '\\';strSize++;}
<STRINGSC>.				{buffer[strSize] = yytext[0]; strSize++;}
<ESCAPE>\"				{BEGIN(STRINGSC); buffer[strSize] = '\"';strSize++;}
<ESCAPE>\'				{BEGIN(STRINGSC); buffer[strSize] = '\'';strSize++;}
<ESCAPE>\\				{BEGIN(STRINGSC); buffer[strSize] = '\\';strSize++;}
<ESCAPE>n				{BEGIN(STRINGSC); buffer[strSize] = '1';strSize++; buffer[strSize] = '2';strSize++;}
<ESCAPE>b				{BEGIN(STRINGSC); buffer[strSize] = '1';strSize++; buffer[strSize] = '0';strSize++;}
<ESCAPE>f				{BEGIN(STRINGSC); buffer[strSize] = '1';strSize++; buffer[strSize] = '4';strSize++;}
<ESCAPE>r				{BEGIN(STRINGSC); buffer[strSize] = '1';strSize++; buffer[strSize] = '5';strSize++;}
<ESCAPE>t				{BEGIN(STRINGSC); buffer[strSize] = '1';strSize++; buffer[strSize] = '1';strSize++;}
<ESCAPE>v				{BEGIN(STRINGSC); buffer[strSize] = '1';strSize++; buffer[strSize] = '3';strSize++;}
<ESCAPE>0				{BEGIN(STRINGSC); buffer[strSize] = '0'; strSize++;}
<ESCAPE>a				{BEGIN(STRINGSC); buffer[strSize] = '\a';strSize++;}
<ESCAPE>\?				{BEGIN(STRINGSC); buffer[strSize] = '\?';strSize++;}
<ESCAPE>[0-7]{1,3}			{BEGIN(STRINGSC); unsigned int temp = strtoul(yytext,NULL,8); buffer[strSize] = temp; strSize++;}
<ESCAPE>x[0-9a-fA-F]+			{BEGIN(STRINGSC);
					if (strlen(yytext) > 3) {
						buffer[strSize] = 0xFFU;
						fprintf(stderr, "%s:%d:Warning:Hex escape sequence \\%s out of range\n", filename, linenum, yytext);
					}
					else {
						
						unsigned int temp = strtoul(yytext+1, NULL, 16);
						buffer[strSize] = temp;
					}
					strSize++;
					}
<FIRSTCHAR>\'				{BEGIN(INITIAL); return CHARLIT;}
<FIRSTCHAR>\\				{BEGIN(NEXTESCAPE);}
<FIRSTCHAR>\n				{fprintf(stderr, "%s:%d:forgot quote\n", filename, linenum); BEGIN(INITIAL);}
<FIRSTCHAR>.				{BEGIN(NEXTCHAR); buffer[strSize] = yytext[0]; strSize++;}
<NEXTESCAPE>\"				{BEGIN(NEXTCHAR); buffer[strSize] = '\"'; strSize++;}
<NEXTESCAPE>\'				{BEGIN(NEXTCHAR); buffer[strSize] = '\''; strSize++;}
<NEXTESCAPE>\\				{BEGIN(NEXTCHAR); buffer[strSize] = '\\'; strSize++;}
<NEXTESCAPE>n				{BEGIN(NEXTCHAR); buffer[strSize] = '\n'; strSize++;}
<NEXTESCAPE>b				{BEGIN(NEXTCHAR); buffer[strSize] = '\b'; strSize++;}
<NEXTESCAPE>f				{BEGIN(NEXTCHAR); buffer[strSize] = '\f'; strSize++;}
<NEXTESCAPE>r				{BEGIN(NEXTCHAR); buffer[strSize] = '\r'; strSize++;}
<NEXTESCAPE>t				{BEGIN(NEXTCHAR); buffer[strSize] = '\t'; strSize++;}
<NEXTESCAPE>v				{BEGIN(NEXTCHAR); buffer[strSize] = '\v'; strSize++;}
<NEXTESCAPE>0				{BEGIN(NEXTCHAR); buffer[strSize] = '\0'; strSize++;}
<NEXTESCAPE>a				{BEGIN(NEXTCHAR); buffer[strSize] = '\a'; strSize++;}
<NEXTESCAPE>\?				{BEGIN(NEXTCHAR); buffer[strSize] = '\?'; strSize++;}
<NEXTESCAPE>[0-7]{1,3}			{BEGIN(NEXTCHAR); unsigned int temp = strtoul(yytext,NULL,8); buffer[strSize] = temp; strSize++;}
<NEXTESCAPE>x[0-9a-fA-F]+		{BEGIN(NEXTCHAR);
					if (strlen(yytext) > 3) {
						buffer[strSize] = 0xFFU;
						fprintf(stderr, "%s:%d:Warning:Hex escape sequence \\%s out of range\n", filename, linenum, yytext);
					}
					else {
						unsigned int temp = strtoul(yytext+1, NULL, 16);
						buffer[strSize] = temp;
					}
					strSize++;
					}
<NEXTCHAR>\'				{BEGIN(INITIAL); yylval.stringVal = buffer; return CHARLIT;}
<NEXTCHAR>\n				{fprintf(stderr, "%s:%d:forgot quote\n", filename, linenum); BEGIN(INITIAL);}
<NEXTCHAR>.				{BEGIN(CHAROVERFLOW);}
<CHAROVERFLOW>\'			{BEGIN(INITIAL); yylval.stringVal = buffer; 
					 fprintf(stderr,"%s:%d:Warning:Unsupported multibyte character literal truncated to first byte\n", filename, linenum); 
					 return CHARLIT;}
<CHAROVERFLOW>\n			{fprintf(stderr, "%s:%d:forgot quote\n", filename, linenum); BEGIN(INITIAL);}
<CHAROVERFLOW>\0			{fprintf(stderr, "%s:%d:forgot quote\n", filename, linenum); BEGIN(INITIAL);}
<CHAROVERFLOW>.				{;}

\0					{return TOKEOF;}
-\>					{return INDSEL;}
\+\+					{return PLUSPLUS;}
--					{return MINUSMINUS;}
\<\<					{return SHL;}
\>\>					{return SHR;}
\<=					{return LTEQ;}
\>=					{return GTEQ;}
==					{return EQEQ;}
!=					{return NOTEQ;}
&&					{return LOGAND;}
\|\|					{return LOGOR;}
\.\.\.				{return ELLIPSIS;}
\*=					{return TIMESEQ;}
\/=					{return DIVEQ;}
%=					{return MODEQ;}
\+=					{return PLUSEQ;}
-=					{return MINUSEQ;}
&=					{return ANDEQ;}
\|=					{return OREQ;}
\^=					{return XOREQ;}
\<\<=				{return SHLEQ;}
\>\>=				{return SHREQ;}
auto					{return AUTO;}
break				{return BREAK;}
case					{return CASE;}
char					{return CHAR;}
const				{return CONST;}
continue				{return CONTINUE;}
default				{return DEFAULT;}
do					{return DO;}
double				{return DOUBLE;}
else					{return ELSE;}
enum					{return ENUM;}
extern				{return EXTERN;}
float				{return FLOAT;}
for					{return FOR;}
goto					{return GOTO;}
if					{return IF;}
inline				{return INLINE;}
int					{return INT;}
long					{return LONG;}
register				{return REGISTER;}
restrict				{return RESTRICT;}
return				{return RETURN;}
short				{return SHORT;}
signed				{return SIGNED;}
sizeof				{return SIZEOF;}
static				{return STATIC;}
struct				{return STRUCT;}
switch				{return SWITCH;}
typedef				{return TYPEDEF;}
union				{return UNION;}
unsigned				{return UNSIGNED;}
void					{return VOID;}
volatile				{return VOLATILE;}
while				{return WHILE;}
_Bool				{return _BOOL;}
_Complex				{return _COMPLEX;}
_Imaginary			{return _IMAGINARY;}
(0|([1-9][0-9]*)|(0[0-7]+)|(0x[0-9a-fA-F]+))([uU][lL][lL]|[lL][lL][uU]|[uU]|[lL]{0,2})			{yylval.number.intVal = strtol(yytext, NULL, 0); 
	strcpy(myType,"INTEGER"); strcpy(yylval.number.myType,"INTEGER");	
	int place = strlen(yytext) - 1;
	while(1) {
		switch(yytext[place]) {
			case 'l':
			case 'L': if (!strcmp("LONG", mySize)) {strcat(mySize,"LONG"); strcat(yylval.number.mySize,"LONG");}
				  else {strcpy(mySize,"LONG"); strcpy(yylval.number.mySize,"LONG");}
				  break;
			case 'u':
			case 'U': strcpy(myUnsign,"UNSIGNED,"); strcpy(yylval.number.myUnsign,"UNSIGNED,");
				  break;
			default:
				if (strcmp("LONG", mySize) && strcmp("LONGLONG", mySize)) {strcpy(mySize,"INT"); strcpy(yylval.number.mySize,"INT");}
				return NUMBER;
		}
		--place;
	}
	return NUMBER; }
((([0-9]*\.[0-9]+|[0-9]+\.)([eE][\+-]?[0-9]+)?)|((0x[0-9a-fA-F]+)([pP][\+-]?[0-9]+)?))[fFlL]?		{yylval.number.floatVal = strtof(yytext, NULL);
	strcpy(myType,"REAL"); strcpy(yylval.number.myType,"REAL");
	int place = strlen(yytext) - 1;
	while(1) {
		switch(yytext[place]) {
			case 'l':
			case 'L': strcpy(mySize,"LONGDOUBLE"); strcpy(yylval.number.mySize,"LONGDOUBLE");
				  break;
			case 'f':
			case 'F': strcpy(mySize,"FLOAT"); strcpy(yylval.number.mySize,"FLOAT");
				  break;
			default:
				if (strcmp("LONGDOUBLE", mySize) && strcmp("FLOAT", mySize)) {strcpy(mySize,"DOUBLE"); strcpy(yylval.number.mySize,"DOUBLE");}
				return NUMBER;
		}
		--place;
	}
	return NUMBER; }
[0-9]+([eE][\+-]?[0-9]+)[fFlL]?	{yylval.number.floatVal = strtof(yytext, NULL);
	strcpy(myType,"REAL"); strcpy(yylval.number.myType,"REAL");
	int place = strlen(yytext) - 1;
	while(1) {
		switch(yytext[place]) {
			case 'l':
			case 'L': strcpy(mySize,"LONGDOUBLE"); strcpy(yylval.number.mySize,"LONGDOUBLE");
				  break;
			case 'f':
			case 'F': strcpy(mySize,"FLOAT"); strcpy(yylval.number.mySize,"FLOAT");
				  break;
			default:
				if (strcmp("LONGDOUBLE", mySize) && strcmp("FLOAT", mySize)) {strcpy(mySize,"DOUBLE"); strcpy(yylval.number.mySize,"DOUBLE");}
				return NUMBER;
		}
		--place;
	}
	return NUMBER; }
[a-zA-Z_][a-zA-Z0-9_]*		{yylval.stringVal = strdup(yytext); symrec* temp = findsym2(yylval.stringVal,0);
							if (temp == NULL || temp->type != TYPEDEFINT) return IDENT; else return TYPEDEFNAME;}
[\n]             			{++linenum;}
[ \t\r\v]+        		  		{/* ignore whitespace */;}
\<\:						{return '[';}
\:\>						{return ']';}
\<\%						{return '{';}
\%\>						{return '}';}
\%\:						{return '#';}
[-\[\]\(\)\{\}\.\&\*\+\~\!\?\%\?\:\;=\,\#\^\|\<\>\/]	{return yytext[0];}
.							{return ERRARE;}


%%

void printString(unsigned char* s, FILE* output) {
	int i;
	for (i = 0; i < strSize; ++i) {
		switch(s[i]) {
			case '\0': fprintf(output, "\\0");
				break;
			case '\n': fprintf(output, "\\n");
				break;
			case '\a': fprintf(output, "\\a");
				break;
			case '\b': fprintf(output, "\\b");
				break;
			case '\f': fprintf(output, "\\f");
				break;
			case '\r': fprintf(output, "\\r");
				break;
			case '\t': fprintf(output, "\\t");
				break;
			case '\v': fprintf(output, "\\v");
				break;
			case '\'': fprintf(output, "\\\'");
				break;
			case '\"': fprintf(output, "\\\"");
				break;
			case '\\': fprintf(output, "\\\\");
				break;
			default:
				if (s[i] >= 32 && s[i] <= 126) fprintf(output, "%c", s[i]);
				else fprintf(output, "\\%03o", s[i]);
		}
	}
}

void printChar(unsigned char* s, FILE* output) {
	switch(s[0]) {
		case '\0': fprintf(output, "\\0");
			break;
		case '\n': fprintf(output, "\\n");
			break;
		case '\a': fprintf(output, "\\a");
			break;
		case '\b': fprintf(output, "\\b");
			break;
		case '\f': fprintf(output, "\\f");
			break;
		case '\r': fprintf(output, "\\r");
			break;
		case '\t': fprintf(output, "\\t");
			break;
		case '\v': fprintf(output, "\\v");
			break;
		case '\'': fprintf(output, "\\\'");
			break;
		case '\"': fprintf(output, "\\\"");
			break;
		case '\\': fprintf(output, "\\\\");
		default:
			if (s[0] >= 32 && s[0] <= 126) fprintf(output, "%c", s[0]);
			else fprintf(output, "\\03o", s[0]);
	}
}

