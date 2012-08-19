%{

#include "stdio.h"
#include "math.h"
#include "calc.h"
#include "calcfnc.h"
#include "string.h"
#include "global.h"
#include "ast.h"
#include "grammar.tab.h"
#include "ir.h"
#include "target.h"

void yyerror(char*s) {
}

int yydebug;

symrec* findsym(char* b, int namespace) { 
	int i; symrec* temp;
	for(i = curScope; i >=0; --i) {
		temp = lookup(all_sym[i][namespace], b); 
		if (temp != NULL) break;
	}
	
	return temp;
}

int mySizeOf(int temp) {
	return 4;
}

struct stack* push(struct stack* myStack, char *val) {
	struct stack* someStack = calloc(1, sizeof(struct stack));
	someStack->type = strdup(val);
	someStack->next = myStack;
	return someStack;
}

char *intToString(ast* myAST) {

	int temp = myAST->type;
	int temp2 = myAST->sign;
	char* s1, *s2, *s3;
	s3 = malloc(50);
	int skip = 0;
	switch(temp) {
		case 0:
		case INT: s1 = "int";
				break;
		case CHAR: s1 = "char";
				break;
		case STRUCT: s1 = "struct";
				break;
		case UNION: s1 = "union";
				break;
		case FLOAT: s1 = "float";
				break;
		case FUNCTION: s1 = "function";
				break;
		case ARRAY: s1 = "array";
				break;
		case POINTER: s1 = "pointer";
				break;
		case TYPEDEFNAME: return myAST->name;
				break;
		case VOID: s1 = "void";
				break;
		case SHORT: s1 = "short int";
				break;
		case SHORTINT: s1 = "short int";
				break;
		case LONG: s1 = "long int";
				break;
		case LONGINT: s1 = "long int";
				break;
		case LONGLONG: s1 = "long long int";
				break;
		case LONGLONGINT: s1 = "long long int";
				break;
		case DOUBLE: s1 = "double";
				break;
		case LONGDOUBLE: s1 = "long double";
				break;
		case SIGNED: s1 = "signed int";
				break;
		case UNSIGNED: s1 = "unsigned int";
				break;
		case ENUM: s1 = "enum";
				break;
		case PLUS: s1 = "sign +";
				break;
		case MINUS: s1 = "sign -";
				break;
		case SHL: s1 = "(<< operator)";
				break;
		case SHR: s1 = "(>> operator)";
				break;
		case PLUSPLUS: s1 = "(++ operator)";
				break;
		case MINUSMINUS: s1 = "(-- operator)";
				break;
		case INDSEL: s1 = "(-> operator)";
				break;
		case EQEQ: s1 = "(== comparator)";
				break;
		case IF: s1 = "IF";
				break;
		case ELSE: s1 = "ELSE";
				break;
		case DO: s1 = "DO WHILE";
				break;
		case WHILE: s1 = "WHILE";
				break;
		case FOR: s1 = "FOR";
				break;
		case COND: s1 = "COND:";
				break;
		case BODY: s1 = "BODY:";
				break;
		case INIT: s1 = "INIT:";
				break;
		case INCR: s1 = "INCR:";
				break;
		case ARG: s1 = "arg";
				break;
		case DEREF: s1 = "DEREF";
				break;
		case BREAK: s1 = "break";
				break;
		case CONTINUE: s1 = "continue";
				break;		
		case RETURN: s1 = "return";
				break;
		case '?': s1 = "(? : operator)";
				break;
		case '(': s1 = "FNCALL";
				break;
		case -1: return "ERROR";
		default: skip = 1;
	}
	
	if(temp2 == SIGNED) {
		switch(temp) {
			case 0:
			case INT:
			case LONGINT:
			case LONGLONGINT:
			case SHORTINT:
			case SHORT:
			case LONG:
			case LONGLONG: s2 = "signed"; 
						break;
			default: temp2 = -1;
				break;
		}
	}
	else if(temp2 == UNSIGNED) {
		switch(temp) {
			case 0:
			case INT:
			case LONGINT:
			case LONGLONGINT:
			case SHORTINT:
			case SHORT:
			case LONG:
			case LONGLONG: s2 = "unsigned";
						break;
			default: temp2 = -1;
			break;
		}
	}
	
	if (skip == 0) {
		if (temp2 == 0)
			return strdup(s1);
		else if (temp2 == -1)
			return "ERROR";
		else {
			sprintf(s3, "%s %s", s2, s1);
			return strdup(s3);
		}
	}

	char s[2];
		sprintf(s, "(%c operator)", temp);
	return strdup(s);
}

void checkStruct(ast* myAST) {
	symrec *temp = findsym(myAST->name, 1);
	if (temp != NULL && temp->type == 0)
		;//fprintf(myfile, "%s %s (definition incomplete)\n", intToString(myAST), myAST->name);
	else
		;//fprintf(myfile, "%s %s (defined %s:%d)\n", intToString(myAST), myAST->name, myAST->filename, myAST->linenum);
}

void addNewScope() {
	curScope++; 
	if (curScope >= curSize) {
		curSize *= 2;
		all_sym = (symrec***) realloc(all_sym, curSize);
	}
	all_sym[curScope] = (symrec**) calloc(3, sizeof(symrec*));
	all_sym[curScope][0] = symtable();
	all_sym[curScope][1] = symtable();
	all_sym[curScope][2] = symtable();
}

void unfurl(ast* declaration) {
	ast* myAST = declaration;
	
	symrec* temp = lookup(all_sym[curScope][0], myAST->name);
	if (temp == NULL) {
		temp = insert(all_sym[curScope][0], myAST->name, 1);
		temp->ast = myAST;
		if (myAST->typeAST->type == ARRAY) {
			myAST->typeAST->type = POINTER;
			myAST->typeAST->name = "*";
		}
	}
	
}

void structMembers(ast* decList, ast* theStructUnion) {
	while(decList != NULL) {
		decList = decList->next;
	}
}

void leaveForStack() {
	struct stack* temp; temp = stack; stack = stack->next; free(temp->type); free(temp);
}

void printTree(ast* myAST, int i) {
	if (myAST == NULL)
		return;
	
	int j = 0;
	/*
	for (j = 0; j < i; ++j) {
		fprintf(myfile, "\t");
	}*/
	
	if (myAST->type == VARIABLE) {
		symrec * temp;
		char *s1 = NULL, *s2 = NULL;
		int i1 = 0, i2 = 0;
		temp = findsym(myAST->name, 0);
		if (temp != NULL) {
			s1 = temp->ast->filename;
			s2 = temp->ast->name;
			i1 = temp->ast->value.intVal;
			i2 = temp->ast->type;
			myAST->typeAST = temp->ast->typeAST;
			myAST->offset = temp->ast->offset;
		}
		s1 = myAST->filename;
		i1 = myAST->linenum;
		//fprintf(myfile, "stab_var name=%s def @ %s:%d\n", myAST->name, s1, i1);
		//if (myAST->typeAST != NULL)
		//	fprintf(myfile, "type=%s\n", intToString(myAST->typeAST));
	}
	
	if (myAST->left != NULL)
		printTree(myAST->left, i + 1);
	if (myAST->right != NULL)
		printTree(myAST->right, i + 1);
	if (myAST->right2 != NULL)
		printTree(myAST->right2, i + 1);
	
	if (myAST->next != NULL) {
		//fprintf(myfile, "\n\n");
		printTree(myAST->next, i);
	}
}

void astDump(ast* fnc, ast* myAST) {
	//fprintf(myfile, "AST dump for function %s\n{\n", fnc->name);
	
	printTree(myAST, 1);

	//fprintf(myfile, "}\n\n");
}

void link(ast* one, ast* two, char* str) {
	if (!strcmp(str, "l")) {
		if (one != NULL)
			one->left = two;
		if (two != NULL)
			two->up = one;
	}
	else if (!strcmp(str, "r")) {
		if (one != NULL)
			one->right = two;
		if (two != NULL)
			two->up = one;
	}
	else if (!strcmp(str, "r2")) {
		if (one != NULL)
			one->right2 = two;
		if (two != NULL)
			two->up = one;
	}
	else if (!strcmp(str, "n")) {
		if (one != NULL)
			one->next = two;
		if (two != NULL)
			two->last = one;
	}
}

ast* link2(ast* one, int blah, char* name, char* str) {
	ast* two = ast_new(name, blah);
	
	if (!strcmp(str, "l")) {
		if (one != NULL)
			one->left = two;
		two->up = one;
	}
	else if (!strcmp(str, "r")) {
		if (one != NULL)
			one->right = two;
		two->up = one;
	}
	else if (!strcmp(str, "r2")) {
		if (one != NULL)
			one->right2 = two;
		two->up = one;
	}
	else if (!strcmp(str, "n")) {
		if (one != NULL)
			one->next = two;
		two->last = one;
	}
	
	return two;
}

ast* astLoop(int type, ast* init, ast* cond, ast* incr, ast* body) {
	ast* dollar = ast_new("", type);
	ast* thisinit;
	ast* thiscond;
	ast* thisincr;
	ast* thisbody;
	
	if (type == FOR) {
		thisinit = link2(dollar, INIT, "", "l");
		thiscond = link2(dollar->left, COND, "", "n");
		thisincr = link2(dollar->left->next, INCR, "", "n");
		thisbody = link2(dollar->left->next->next, BODY, "", "n");
		link(thisinit, init, "l");
		link(thiscond, cond, "l");
		link(thisincr, incr, "l");
		link(thisbody, body, "l");
	}
	else {
		thiscond = link2(dollar, COND, "", "l");
		thisbody = link2(dollar->left, BODY, "", "n");
		link(thiscond, cond, "l");
		link(thisbody, body, "l");
	}
	
	return dollar;
}

%}

%token TOKEOF
%token <stringVal> IDENT
%token <stringVal> CHARLIT
%token <stringVal> STRING
%token <number> NUMBER
%token INDSEL
%token PLUSPLUS
%token MINUSMINUS
%token SHL
%token SHR
%token LTEQ
%token GTEQ
%token EQEQ
%token NOTEQ
%token LOGAND
%token LOGOR
%token ELLIPSIS
%token TIMESEQ
%token DIVEQ
%token MODEQ
%token PLUSEQ
%token MINUSEQ
%token SHLEQ
%token SHREQ
%token ANDEQ
%token OREQ
%token XOREQ
%token AUTO
%token BREAK
%token CASE
%token CHAR
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EXTERN
%token FLOAT
%token FOR
%token GOTO
%token IF
%token INLINE
%token INT
%token LONG
%token LONGINT
%token LONGLONG
%token LONGLONGINT
%token LONGDOUBLE
%token REGISTER
%token RESTRICT
%token RETURN
%token SHORT
%token SHORTINT
%token SIGNED
%token SIZEOF
%token STATIC
%token STRUCT
%token SWITCH
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE
%token _BOOL
%token _COMPLEX
%token <stringVal> TYPEDEFNAME
%token _IMAGINARY
%token ERRARE
%token NEWLINE
%token BOGUS
%token ARRAY
%token FUNCTION
%token POINTER
%token VARIABLE
%token CAST
%token DEREF
%token ADDRESS
%token PLUS
%token MINUS
%token MEMBER
%token COND
%token INIT
%token BODY
%token INCR
%token ARG
%token TYPEDEFINT
%token TEMPORARY
%token DIRECT
%token INDIRECT
%token LABEL
%token BLANK

%token ADD
%token SUB
%token LEA
%token MUL
%token STORE
%token LOAD
%token MOV
%token DIV
%token ADDRVAL
%token CMP
%token BRGE
%token CALL
%token PASSARG
%token MOVSTR

%right '='
%left '+' '-'
%left '*' '/'
%left IF
%left ELSE
%nonassoc REDUCE
%nonassoc '('

%union {
	int intVal;
	float floatVal;
	unsigned char* stringVal;
	char** multiString;
	int operator;
	struct ast *myAST;
	struct numberStruct {
		int intVal;
		float floatVal;
		char myUnsign[256];
		char myType[256];
		char mySize[256];
	} number;
}

%type <myAST> primaryExpression
%type <myAST> postfixExpression
%type <myAST> unaryExpression
%type <myAST> castExpression
%type <myAST> multExpression
%type <myAST> addExpression
%type <myAST> shiftExpression
%type <myAST> relExpression
%type <myAST> equalExpression
%type <myAST> ANDExpression
%type <myAST> xORExpression
%type <myAST> ORExpression
%type <myAST> lANDExpression
%type <myAST> lORExpression
%type <myAST> condExpression
%type <myAST> assignExpression
%type <myAST> expression
%type <myAST> expressionStatement
%type <myAST> constantExpression
%type <myAST> statement
%type <operator> unaryOperator
%type <operator> assignOperator

%type <myAST> blockItem
%type <myAST> blockItemList
%type <myAST> compoundStatementMiddle
%type <myAST> compoundStatement
%type <myAST> selectStatement
%type <myAST> labelStatement
%type <myAST> jumpStatement
%type <myAST> iterStatement
%type <myAST> argExpressionList

%type <intVal> structOrUnion
%type <myAST> structOrUnionSpec
%type <myAST> structOrUnionSpecBegins
%type <myAST> structOrUnionSpecMiddle
%type <intVal> storClassSpec
%type <myAST> typeSpec
%type <stringVal> typeQual
%type <myAST> declarationSpec
%type <myAST> directDeclarator
%type <myAST> declarator
%type <myAST> initDeclarator
%type <myAST> initDeclaratorList
%type <stringVal> funcSpec
%type <myAST> pointer
%type <stringVal> typeQualList
%type <stringVal> typeDefName
%type <myAST> functionDefBegins
%type <myAST> functionDefBeginsScope
%type <myAST> functionDefMiddle
%type <myAST> structDeclarator
%type <myAST> structDeclaratorList
%type <myAST> specQualList
%type <myAST> structDeclaration
%type <myAST> structDeclarationList
%type <myAST> enumConstant
%type <myAST> enumerator
%type <myAST> enumList
%type <myAST> declaration
%type <myAST> paramTypeList
%type <myAST> paramList
%type <myAST> paramDeclaration
%type <stringVal> identOrTypedefname

%%

translationUnit: /*empty*/ {;}
	| translationUnit externDeclaration

	| error
	;
	
identOrTypedefname: IDENT {$$= $1;}
					| TYPEDEFNAME {$$ = $1;}
					;
	
newScope: '{' {addNewScope(); stack = push(stack, "");}
	;

leaveScope: '}' {freedom(all_sym[curScope][0]); freedom(all_sym[curScope][1]); freedom(all_sym[curScope][2]); free(all_sym[curScope]); curScope--;
				struct stack* temp; temp = stack; stack = stack->next; free(temp->type); free(temp);}
	;

primaryExpression: IDENT {	symrec* temp = findsym($1, 0);
						if (temp != NULL) {
							$$ = ast_new($1, VARIABLE);
							$$->variable = temp;
							
							$$->filename = temp->ast->filename;
							$$->linenum = temp->ast->linenum;
							$$->scope = temp->ast->scope;
						}
						else {
							fprintf(stderr, "%s:%d: ERROR FOR %s\n", filename, linenum, $1);
							$$ = ast_new($1, VARIABLE);
							$$->variable = temp;
						}
					}
	  | NUMBER {	$$ = ast_new("number", NUMBER);
	  			$$->value.intVal = $1.intVal;
	  		}
	  | STRING {	$$ = ast_new($1, STRING);
	  			$$->value.stringVal = $1;
	  		}
	  | CHARLIT {	$$ = ast_new($1, CHARLIT);
	  			$$->value.stringVal = $1;
	  		}
	  | '(' expression ')' {$$ = $2; }
	  ;

postfixExpression: primaryExpression {$$ = $1;}
		| postfixExpression '[' expression ']' {$$ = ast_new("", DEREF);
												ast* temp = ast_new("", '+');
												link($$,temp,"l");
												link(temp,$1,"l");
												link(temp, $3, "r");
												}
		| postfixExpression '(' ')' {$$ = ast_new("", '(');
														link($$,$1,"l");
													}
		| postfixExpression '(' argExpressionList ')' {$$ = ast_new("", '(');
														link($$,$1,"l");
														link($$, $3, "r");
													}
		| postfixExpression '.' IDENT {$$ = ast_new("", '.');
									link($$,$1,"l");
									ast* temp = ast_new($3, MEMBER);
									link($$,temp,"r");
									}
		| postfixExpression INDSEL IDENT {$$ = ast_new("", INDSEL);
									link($$,$1,"l");
									ast* temp = ast_new($3, MEMBER);
									link($$,temp,"r");
									}
		| postfixExpression PLUSPLUS { 	$$ = ast_new("", PLUSPLUS);
									link($$,$1,"l");
								}
	  	| postfixExpression MINUSMINUS { $$ = ast_new("", MINUSMINUS);
										link($$,$1,"l");
									}
		| '(' typeName ')' newScope initializerList leaveScope {$$; }
		| '(' typeName ')' newScope initializerList ',' leaveScope {$$; }
		;

argExpressionList: assignExpression {$$ = ast_new("", ARG); link($$, $1, "l"); $$->value.intVal = 1;}
		 | argExpressionList ',' assignExpression {$$=$1; ast* temp = ast_new("", ARG); 
													connectHor($$, temp); 
													link(temp, $3, "l"); 
													temp->value.intVal = temp->last->value.intVal + 1;}
		 ;

unaryExpression: postfixExpression {$$ = $1; }
		 | PLUSPLUS unaryExpression {	$$ = ast_new("", PLUSPLUS);
										link($$,$2,"l");
									}
	  	 | MINUSMINUS unaryExpression { $$ = ast_new("", MINUSMINUS);
										link($$,$2,"l");
									}
	  	 | unaryOperator castExpression {	$$ = ast_new("", $1);
											link($$,$2,"l");
										}
		 | SIZEOF unaryExpression {$$ = ast_new("", SIZEOF);
										link($$,$2,"l");}
		 | SIZEOF '(' typeName ')' {$$; }
		 ;

unaryOperator: '&' {$$ = ADDRESS;}| '*' {$$ = DEREF;}| '+' {$$ = PLUS;}| '-' {$$ = MINUS;}| '~' {$$ = '~';}| '!' {$$ = '!';}
	     ;

castExpression: unaryExpression {$$ = $1; }
		| '(' typeName ')' castExpression {$$ = $4; }
		;

multExpression: castExpression {$$ = $1; }
		| multExpression '*' castExpression {	$$ = ast_new("", '*'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| multExpression '/' castExpression {	$$ = ast_new("", '/'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| multExpression '%' castExpression {	$$ = ast_new("", '%'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

addExpression: multExpression {$$ = $1; }
		| addExpression '+' multExpression {$$ = ast_new("", '+'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| addExpression '-' multExpression {$$ = ast_new("", '-'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

shiftExpression: addExpression {$$ = $1; }
		| shiftExpression SHL addExpression {$$ = ast_new("", SHL); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| shiftExpression SHR addExpression {$$ = ast_new("", SHR); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

relExpression: shiftExpression {$$ = $1; }
		| relExpression '<' shiftExpression {$$ = ast_new("", '<'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| relExpression '>' shiftExpression {$$ = ast_new("", '>'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| relExpression LTEQ shiftExpression {$$ = ast_new("", LTEQ); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| relExpression GTEQ shiftExpression {$$ = ast_new("", GTEQ); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

equalExpression: relExpression {$$ = $1; }
		| equalExpression EQEQ shiftExpression {$$ = ast_new("", EQEQ); 
							link($$,$1,"l");
							link($$,$3,"r");}
		| equalExpression NOTEQ shiftExpression {$$ = ast_new("", NOTEQ); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

ANDExpression: equalExpression {$$ = $1; }
		| ANDExpression '&' equalExpression {$$ = ast_new("", '&'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

xORExpression: ANDExpression {$$ = $1; }
		| xORExpression '^' ANDExpression {$$ = ast_new("", '^'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

ORExpression: xORExpression {$$ = $1; }
		| ORExpression '|' xORExpression {$$ = ast_new("", '|'); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

lANDExpression: ORExpression {$$ = $1; }
		| lANDExpression LOGAND ORExpression {$$ = ast_new("", LOGAND); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

lORExpression: lANDExpression {$$ = $1; }
		| lORExpression LOGOR lANDExpression {$$ = ast_new("", LOGOR); 
							link($$,$1,"l");
							link($$,$3,"r");}
		;

condExpression: lORExpression {$$ = $1; }
		| lORExpression '?' expression ':' condExpression {$$ = ast_new("", '?'); 
							link($$,$1,"l");
							link($$,$3,"r");
							link($$,$5,"r2");}
		;

assignExpression: condExpression {$$ = $1; }
		| unaryExpression assignOperator assignExpression { 
								if ($2 == '=') {
									$$ = ast_new("", '='); 
									link($$,$1,"l");
									link($$,$3,"r");
								}
								else {
									ast* temp = ast_new("", $2); 
									link(temp,$1,"l");
									link(temp,$3,"r");
									
									$$ = ast_new("", '='); 
									link($$,$1,"l");
									link($$,temp,"r");
								}
							}
		;

assignOperator: '=' {$$ = '=';} | TIMESEQ {$$ = '*';} | DIVEQ {$$ = '/';} 
		| MODEQ {$$ = '%';} | PLUSEQ {$$ = '+';} | MINUSEQ {$$ = '-';}
		| SHLEQ {$$ = SHL;} | SHREQ {$$ = SHR;} | ANDEQ {$$ = '&';} 
		| XOREQ {$$ = '^';} | OREQ {$$ = '|';}
		;

expression: assignExpression {$$ = $1; }
	  | expression ',' assignExpression {$$ = ast_new("", ','); 
									link($$,$1,"l");
									link($$,$3,"r");;}
	  ;

constantExpression: condExpression {$$ = $1;}
		;

declaration: declarationSpec ';' { /*fprintf(myfile, "%s:%d: There is %s here\n\n", filename, linenum, intToString($1));*/ }
	   | declarationSpec initDeclaratorList ';' {	$$ = $2;
	   										ast* temp = $2;
	   										
	   										if (($1->type == STRUCT || $1->type == UNION) && findsym($1->name, 1) == NULL)
	   											;
	   										else {
												while(temp != NULL) {
													connectDown(&(temp->typeAST),$1);
													
													if (curScope != 0 && $1->storage != EXTERN) {
														localoffset -= temp->typeAST->size;
														temp->offset = localoffset;
													}
													else {
														temp->offset = 0;
														if (temp->typeAST->type != FUNCTION)
															fprintf(commfile, "\t.comm \t_%s, 4, 4\n", temp->name);
													}
														
													unfurl(temp);
													temp = temp->next;
												}
											}
										 }
	 	| TYPEDEF typeSpec IDENT ';' {		symrec* temp = lookup(all_sym[curScope][0], $3);
										if (temp == NULL) {
											$$ = ast_new($3, TYPEDEF);
											$$->left = $2;
											$$->size = $2->size;
											insert(all_sym[curScope][0], $3, TYPEDEFINT)->ast = $$;
											//fprintf(myfile, "%s:%d: typedef name \"%s\" defined here in %s scope, as type:\n\t%s\n\n", filename, linenum, $3, stack->type, intToString($2));
										}
										else {
											;//fprintf(stderr, "ERROR: %s:%d: typedef name \"%s\" redefined in %s scope\n\n", filename, linenum, $3, stack->type);
										}
									}
		| TYPEDEF typeSpec TYPEDEFNAME ';' {//fprintf(stderr, "ERROR: %s:%d: typedef name \"%s\" redefined in %s scope\n\n", filename, linenum, $3, stack->type); 
											$$ = NULL;}
		| ERRARE {$$; YYACCEPT;}
	   ;

declarationSpec: storClassSpec {$$ = ast_new("", 0); $$->storage = $1;}
		| typeSpec {$$ = $1;}
		| typeQual {$$ = ast_new("", 0);}
		| funcSpec {$$ = ast_new($1, 0);}
		| storClassSpec declarationSpec {$$ = $2; $$->storage = $1;}
		| typeSpec declarationSpec {$$ = $2; 
								switch($$->type) {
									case 0: $$->type = $1->type;
											break;
									case INT: if ($1->type == LONG)
												$$->type = LONGINT;
											 else if ($1->type == LONGLONG)
											 	$$->type = LONGLONGINT;
											 else if ($1->type == SHORT)
											 	$$->type = SHORTINT;
											 else if ($1->sign == SIGNED)
											 	$$->sign = SIGNED;
											 else if ($1->sign == UNSIGNED)
											 	$$->sign = UNSIGNED;
											 else
											 	$$->type = -1;
											break;
									case LONG: if ($1->type == LONG)
												$$->type = LONGLONG;
											 else if ($1->type == INT)
											 	$$->type = LONGINT;
											 else if ($1->type == LONGINT)
											 	$$->type = LONGLONGINT;
											 else if ($1->type == DOUBLE)
											 	$$->type = LONGDOUBLE;
											 else if ($1->sign == SIGNED)
											 	$$->sign = SIGNED;
											 else if ($1->sign == UNSIGNED)
											 	$$->sign = UNSIGNED;
											 else
											 	$$->type = -1;
											break;
									case LONGINT: if ($1->type == LONG)
												$$->type = LONGLONGINT;
												else if ($1->sign == SIGNED)
													$$->sign = SIGNED;
												else if ($1->sign == UNSIGNED)
												 	$$->sign = UNSIGNED;
											 else
											 	$$->type = -1;
											break;
									case LONGLONGINT: if ($1->sign == SIGNED)
													$$->sign = SIGNED;
												else if ($1->sign == UNSIGNED)
												 	$$->sign = UNSIGNED;
											 else
											 	$$->type = -1;
											break;
									case LONGLONG: if ($1->type == INT)
											 	$$->type = LONGINT;
											 	else if ($1->sign == SIGNED)
												 	$$->sign = SIGNED;
												 else if ($1->sign == UNSIGNED)
												 	$$->sign = UNSIGNED;
											 else
											 	$$->type = -1;
											break;
									case SHORT: if ($1->type == INT)
											 	$$->type = SHORTINT;
											 	else if ($1->sign == SIGNED)
												 	$$->sign = SIGNED;
												 else if ($1->sign == UNSIGNED)
												 	$$->sign = UNSIGNED;
											 else
											 	$$->type = -1;
											break;
									case SHORTINT: if ($1->sign == SIGNED)
												 	$$->sign = SIGNED;
												 else if ($1->sign == UNSIGNED)
												 	$$->sign = UNSIGNED;
											 else
											 	$$->type = -1;
											break;
									case DOUBLE: if ($1->type == LONG)
												$$->type = LONGDOUBLE;
											 else
											 	$$->type = -1;
											break;
									default: $$->type = -1;
								}
								if ($$->sign == 0 && $1->sign != 0) {
									$$->sign = $1->sign;
								}
								$$->name = $1->name;}
		| typeQual declarationSpec {$$ = $2;}
		| funcSpec declarationSpec {$$ = $2;}
		;

funcSpec: INLINE {$$ = "inline";}
	;

initDeclaratorList: initDeclarator {$$ = $1;}
		  | initDeclaratorList ',' initDeclarator {$$ = $3; $$->next = $1; $1->last = $$;}
		  ;

initDeclarator: declarator {$$ = $1;}
	      | declarator '=' initializer {$$ = $1;}
	      ;

storClassSpec: EXTERN {$$ = EXTERN;}
	     | STATIC {$$ = STATIC;}
	     | AUTO {$$ = AUTO;}
	     | REGISTER {$$ = REGISTER;}
	     ;

typeSpec: VOID {$$ = ast_new("",VOID);}
	| CHAR {$$ = ast_new("",CHAR); $$->size = 4;}
	| SHORT {$$ = ast_new("",SHORT); $$->size = 4;}
	| INT {$$ = ast_new("",INT); $$->size = 4;}
	| LONG {$$ = ast_new("",LONG); $$->size = 4;}
	| FLOAT {$$ = ast_new("",FLOAT); $$->size = 4;}
	| DOUBLE {$$ = ast_new("",DOUBLE); $$->size = 4;}
	| SIGNED {$$ = ast_new("",0); $$->sign = SIGNED;}
	| UNSIGNED {$$ = ast_new("",0); $$->sign = UNSIGNED; }
	| _BOOL {$$ = ast_new("",_BOOL);}
	| _COMPLEX {$$ = ast_new("",_COMPLEX);}
	| structOrUnionSpec {$$ = $1;}
	| enumSpec {$$ = ast_new("",ENUM);}
	| typeDefName {$$ = ast_new($1, TYPEDEFNAME);}
	;

typeDefName: BOGUS {$$;}
		| TYPEDEFNAME {$$ = $1;}
		;

structOrUnionSpec: structOrUnionSpecMiddle leaveScope {$$ = $1;
														if ($$ != NULL) {
															if ($$->type == STRUCT) $$->size = offset; else $$->size = 4;
														}
													}
			| structOrUnion identOrTypedefname {		symrec* temp = findsym($2, 1);
									if (temp == NULL) {
										$$ = ast_new($2,$1);
										$$->value.intVal = 0;
										insert(all_sym[0][1], $2, 0)->ast = $$;
										//fprintf(myfile, "%s:%d: \"%s %s\" left undefined.\n\n", filename, linenum, intToString($$), $2);
									}
									else
										$$ = temp->ast;
								}
			;

structOrUnionSpecMiddle: structOrUnionSpecBegins {$$ = $1;}
				| structOrUnionSpecBegins structDeclarationList { 		$$ = $1; $$->structStuff = $2; 
																if($$->name != NULL) {
																	structMembers($2, $1);
																	//fprintf(myfile, "}\n\n");
																}
																else {
																	$$->name = filename;
																	//fprintf(myfile, "\n");
																}
																symrec *temp = findsym($1->name, 1);
																if (temp != NULL)
																	temp->type = 1;
														}
				;

structOrUnionSpecBegins: structOrUnion newScope {	offset = 0;
										$$ = ast_new("$",$1);
										$$->filename = filename;
										$$->linenum = linenum;
										$$->value.intVal = linenum;
										//fprintf(myfile, "%s:%d: Unnamed struct/union defined here in %s scope {\n", filename, linenum, stack->next->type);
										free(stack->type); stack->type = strdup("structOrUnion");
										}
					| structOrUnion identOrTypedefname newScope {	free(stack->type); stack->type = strdup("structOrUnion");
										offset = 0;
										symrec* temp = findsym($2, 1);
										if (temp == NULL) {
											$$ = ast_new($2,$1);
											$$->filename = filename;
											$$->linenum = linenum;
											$$->value.intVal = linenum;
											insert(all_sym[0][1], $2, 0)->ast = $$;
											//fprintf(myfile, "%s:%d: \"%s %s\" defined here in %s scope {\n", filename, linenum, intToString($$), $2, stack->next->type);
										}
										else if (temp->type == 0) {
											$$ = temp->ast;
											$$->filename = filename;
											$$->linenum = linenum;
											$$->value.intVal = linenum;
											//fprintf(myfile, "%s:%d: \"%s %s\" defined here, previously declared in %s scope {\n", filename, linenum, intToString($$), $2, stack->next->type);
										}
										else {
											$$ = ast_new($2,$1);
											$$->filename = filename;
											$$->linenum = linenum;
											$$->value.intVal = linenum;
											free($$->name);
											$$->name = NULL;
											//fprintf(stderr, "ERROR: %s:%d: struct/union \"%s\" redefined in %s scope\n", filename, linenum, $2, stack->next->type);
										}
									}
		; 
				
structOrUnion: STRUCT {$$ = STRUCT;}
		| UNION	{$$ = UNION;}
		;
				
structDeclarationList: structDeclaration {$$= $1;}
			| structDeclarationList structDeclaration {	$$ = $1;
														if ($2 != NULL && $1 != NULL) connectHor($1, $2);
														else if($2 != NULL) $$ = $2;}
			;
					
structDeclaration: specQualList structDeclaratorList ';' {		$$ = $2;
													ast *temp = $2;
													while(temp != NULL) {
														temp->offset = offset;
														offset += mySizeOf($1->type);
														connectDown(&(temp->typeAST), $1);
														temp = temp->next;
													}
											}
		;
				
specQualList: typeSpec {$$=$1;}
		| typeSpec specQualList {$$=$2; $$->type = $1->type; $$->name = $1->name;}
		| typeQual {$$;}
		| typeQual specQualList {$$ = $2;}
		;
			
structDeclarator: declarator {$$=$1;}
			| ':' constantExpression {$$;}
			| declarator ':' constantExpression {$$=$1;}
			;

structDeclaratorList: structDeclarator {$$= $1;}
			| structDeclaratorList ',' structDeclarator {	$$ = $1;
															if ($3 != NULL && $1 != NULL) connectHor($1, $3);
															else if($3 != NULL) $$ = $3;}
			;

enumSpecBegins: ENUM '{'
	| ENUM IDENT '{'
	;

enumSpec: enumSpecBegins enumList '}'
	| enumSpecBegins enumList ',' '}'
	| ENUM IDENT
	;

enumList: enumerator {$$ = $1;}
	| enumList ',' enumerator {$$ = $3; $$->next = $1; $1->last = $$;}
	;
		
enumerator: enumConstant {$$ = $1;}
	| enumConstant '=' constantExpression {$$ = $1; $$->value.intVal;}
	;
			
enumConstant: IDENT { $$ = ast_new($1,ENUM);}
		;
	
typeQual: CONST {$$ = strdup("const");}
	| RESTRICT {$$ = strdup("restrict");}
	| VOLATILE {$$ = strdup("volatile");}
	;

typeQualList: typeQual {$$ = $1;}
	    | typeQualList typeQual {$$ = $1;}
	    ;

declarator: directDeclarator { $$ = $1; $$->scope = curScope;}
	  | pointer directDeclarator {$$ = $2; $$->scope = curScope;
	  						connectDown(&($$->typeAST),$1);
							}
	  ;

directDeclarator: IDENT { $$ = ast_new($1, IDENT); $$->value.intVal = linenum; $$->filename = filename; $$->linenum = linenum; }
		| '(' declarator ')' { $$ = $2;}
		| directDeclarator '[' typeQualList assignExpression ']' {$$=$1; add_node(&($$->typeAST), arrayString, ARRAY)->value.intVal = $4->value.intVal; $$->typeAST->size *= $4->value.intVal;}
		| directDeclarator '[' assignExpression ']' {$$=$1; add_node(&($$->typeAST), arrayString, ARRAY)->value.intVal = $3->value.intVal; $$->typeAST->size *= $3->value.intVal;}
		| directDeclarator '[' typeQualList ']' { $$ = $1;}
		| directDeclarator '[' ']' {$$=$1; add_node(&($$->typeAST), arrayString, ARRAY)->value.intVal = 0;}
		| directDeclarator '[' STATIC typeQualList assignExpression ']' {$$=$1; add_node(&($$->typeAST), arrayString, ARRAY)->value.intVal = $5->value.intVal;}
		| directDeclarator '[' STATIC assignExpression ']' {$$=$1; add_node(&($$->typeAST), arrayString, ARRAY)->value.intVal = $4->value.intVal;}
		| directDeclarator '[' typeQualList STATIC assignExpression ']' {$$=$1; add_node(&($$->typeAST), arrayString, ARRAY)->value.intVal = $5->value.intVal;}
		| directDeclarator '[' typeQualList '*'']' { $$ = $1;}
		| directDeclarator '[' '*' ']' { $$ = $1;}
		| directDeclarator '(' paramTypeList ')' {$$=$1; $$->param = $3; add_node(&($$->typeAST), functionString, FUNCTION);}
		| directDeclarator '(' identList ')' {$$=$1; add_node(&($$->typeAST), functionString, FUNCTION);}
		| directDeclarator '(' ')' {$$=$1; add_node(&($$->typeAST), functionString, FUNCTION);}
		;

paramTypeList: paramList {$$ = $1;}
		| paramList ',' ELLIPSIS {$$ = $1;}
		;
			
paramList: paramDeclaration {$$ = $1;}
	| paramList ',' paramDeclaration {	$$ = $1;
										if ($3 != NULL && $1 != NULL) connectHor($1, $3);
										else if($3 != NULL) $$ = $3;}
	;
			
paramDeclaration: declarationSpec declarator {$$ = $2; connectDown(&($$->typeAST),$1);
												paramoffset += 4;
												$$->offset = paramoffset;
												$$->scope = -1;}
		;
		
identList: IDENT
	| identList ',' IDENT
	;
				
typeName: specQualList
	| specQualList absDeclarator
	;
		
absDeclarator: pointer
		| directAbsDeclarator
		| pointer directAbsDeclarator
		;
			
directAbsDeclarator: '(' absDeclarator ')'
		| directAbsDeclarator '[' assignExpression ']'
		| '[' assignExpression ']'
		| directAbsDeclarator '['  ']'
		| '['  ']'
		| '[' '*' ']'
		| directAbsDeclarator '[' '*' ']'
		| directAbsDeclarator '(' paramTypeList ')'
		| '(' paramTypeList ')'
		| directAbsDeclarator '('  ')'
		| '(' ')'
		;
					

initializer: assignExpression
	   | newScope initializerList leaveScope
	   | newScope initializerList ',' leaveScope
	   ;

initializerList: initializer
		| designation initializer
		| initializerList ',' initializer
		| initializerList ',' designation initializer
		;

designation: designatorList '='
	   ;

designatorList: designator
		| designatorList designator
		;

designator: '.' IDENT
	| '[' constantExpression ']'
	;
		
pointer: '*' {$$ = ast_new("*",POINTER);}
	| '*' typeQualList {$$ = ast_new("*",POINTER);}
	| '*' pointer {$$=$2; add_node(&$2, "*", POINTER);}
	| '*' typeQualList pointer {$$=$3; add_node(&$3, "*", POINTER);}
	;

statement: expressionStatement {$$ = $1;}
	 | compoundStatement {$$ = $1;}
	 | labelStatement {$$ = $1;}
	 | selectStatement {$$ = $1;}
	 | iterStatement {$$ = $1;}
	 | jumpStatement {$$ = $1;}
	 ;
	
labelStatement: identOrTypedefname ':' statement {	symrec* temp = findsym($1, 2);
													if (temp == NULL) {
														temp = insert(all_sym[curScope][2], $1, LABEL);
													}
													else {
														printf("ERROR: %s:%d: \"%s\" redeclared\n\n", filename, linenum, temp->name);
													}
													$$ = ast_new($1, LABEL);
													temp->ast = $$;
													link($$, $3, "n");
												}
		| CASE constantExpression ':' statement {$$ = $4;}
		| DEFAULT ':' statement {$$ = $3;}
		;

compoundStatementBegins: newScope {free(stack->type); stack->type = strdup("block");}
		;

compoundStatementMiddle: compoundStatementBegins blockItemList {$$ = $2;}
		;

compoundStatement: newScope leaveScope {$$ = (struct ast*) NULL;}
		| compoundStatementMiddle leaveScope {$$ = $1;}
		;

blockItemList: blockItem {$$ = $1;}
		| blockItemList blockItem {	$$ = $1;
						if ($2 != NULL && $1 != NULL) connectHor($1, $2);
						else if($2 != NULL) $$ = $2;}
		;

blockItem: declaration {$$ = (struct ast*) NULL;}
	| statement {$$ = $1;}
	;
	
expressionStatement: expression ';' {$$ = $1; tempNum = 1;}
		   | ';' {$$ = (struct ast*) NULL;}
		   ;

selectStatement: IF '(' expression ')' statement	%prec IF {$$ = ast_new("", IF); 
																link2($$, COND, "", "l");
																link2($$, BODY, "", "r");
																link($$->left,$3, "l"); 
																link($$->right,$5, "l");
																}
		| IF '(' expression ')' statement ELSE statement	%prec ELSE { $$ = ast_new("", IF); 
																link2($$, COND, "", "l");
																link2($$, BODY, "", "r");
																link($$->left,$3, "l"); 
																link($$->right,$5, "l");
																link2($$, ELSE, "", "n");
																link2($$->next, BODY, "", "l");
																link($$->next->left,$7, "l"); 
																}
		| SWITCH '(' expression ')' statement {$$ = ast_new("", SWITCH); 
												link2($$, COND, "", "l");
												link2($$, BODY, "", "r");
												link($$->left,$3, "l"); 
												link($$->right,$5, "l");
												}
		;
				
iterStatement: WHILE '(' expression ')' statement {$$ = astLoop(WHILE, NULL, $3, NULL, $5);}
		| DO statement WHILE '(' expression ')' ';' {$$ = astLoop(DO, NULL, $5, NULL, $2);}
		| FOR '(' expression ';' expression ';' expression ')' statement {$$ = astLoop(FOR, $3, $5, $7, $9);}
		| FOR '(' expression ';' expression ';'  ')' statement {$$ = astLoop(FOR, $3, $5, NULL, $8);}
		| FOR '(' expression ';'  ';'  ')' statement {$$ = astLoop(FOR, $3, NULL, NULL, $7);}
		| FOR '('  ';'  ';'  ')' statement {$$ = astLoop(FOR, NULL, NULL, NULL, $6);}
		| FOR '(' expression ';'  ';' expression ')' statement {$$ = astLoop(FOR, $3, NULL, $6, $8);}
		| FOR '('  ';'  ';' expression ')' statement {$$ = astLoop(FOR, NULL, NULL, $5, $7);}
		| FOR '('  ';' expression ';' expression ')' statement {$$ = astLoop(FOR, NULL, $4, $6, $8);}
		| FOR '('  ';' expression ';'  ')' statement {$$ = astLoop(FOR, NULL, $4, NULL, $7);}
		| FOR '(' declaration expression ';' expression ')' statement {$$;}
		| FOR '(' declaration  ';' expression ')' statement {$$;}
		| FOR '(' declaration expression ';'  ')' statement {$$;}
		| FOR '(' declaration ';'  ')' statement {$$;}
		;
				
jumpStatement: GOTO identOrTypedefname ';' {$$ = ast_new($2, GOTO);}
		| CONTINUE ';' {$$ = ast_new("", CONTINUE);}
		| BREAK ';' {$$ = ast_new("", BREAK);}
		| RETURN expression ';' {$$ = ast_new("", RETURN); link($$,$2, "l"); }
		| RETURN ';' {$$ = ast_new("", RETURN);}
		;

externDeclaration: functionDef
		| declaration
		;

functionDefBegins: declarationSpec declarator {paramoffset = 4; $$ = $2; $$->offset = 0; connectDown(&($$->typeAST), $1); unfurl($$);}
		| declarationSpec declarator declarationList {paramoffset = 4; $$ = $2; $$->offset = 0; connectDown(&($$->typeAST), $1); unfurl($$);}
		| declarator declarationList {paramoffset = 4; $$ = $1; $$->offset = 0; connectDown(&($$->typeAST), ast_new("int",INT)); unfurl($$);}
		| declarator {paramoffset = 4; $$ = $1; $$->offset = 0; connectDown(&($$->typeAST), ast_new("int",INT)); unfurl($$);}
		;
		
functionDefBeginsScope: functionDefBegins newScope {localoffset = 0; $$ = $1; free(stack->type); stack->type = strdup(functionString);
													ast* temp = $1->param;
													while (temp != NULL) {unfurl(temp); temp = temp->next;}
													}
		;
		
functionDefMiddle: functionDefBeginsScope {$$ = $1;}
		| functionDefBeginsScope blockItemList {$1->next = $2; $$ = $1; astDump($1, $2);}
		;

functionDef: functionDefMiddle leaveScope {if ($1 != NULL) fprintf(myfile,"%s:\n.BB%d.%d:\n",$1->name,fncNum,blockNum); 
											irTree($1, NULL, NULL); 
											fprintf(myfile,"\tRETURN");
											blockNum = 1;
											++fncNum;
											fprintf(myfile,"\n\n");
											paramoffset = 4;
											}
		;	
			 
declarationList: declaration
		| declarationList declaration
		;
%%

int yywrap() { return 1; }
extern int yydebug;

int main() {

	tempNum = 1;
	fncNum = 1;
	blockNum = 1;
	localoffset = 0;
	paramoffset = 4;
	
	myfile = fopen ("myfile","w");
	commfile = fopen ("commfile","w");
	
	curSize = 100;
	all_sym = (symrec***) calloc(curSize, sizeof(all_sym));
	functionString = strdup("function");
	declarationString = strdup("declaration");
	arrayString = strdup("array");
	
	int i = 0;
	int j = 0;
	
	//yylex();
	//yylex();
	//linenum = yylval.number.intVal - 4;
	//yylex();
	//strcpy(filename,yylval.stringVal);
	
	linenum = 0;
	strcpy(filename,"blah.c");

	curScope = 0;
	stack = push(stack, "global");
	stack->next = NULL;

	all_sym[curScope] = (symrec**) calloc(3, sizeof(symrec*));
	all_sym[curScope][0] = symtable();
	all_sym[curScope][1] = symtable();
	all_sym[curScope][2] = symtable();
	
	yydebug = 1;
	yyparse();
	
	fclose (myfile);
	generate();
}
