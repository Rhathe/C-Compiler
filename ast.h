#ifndef AST_H
#define AST_H

#include "string.h"
#include "stdlib.h"

#define AST_IDENT 0
#define AST_NUM 1
#define AST_FNC 2
#define AST_

struct ast {
	struct ast *left;
	struct ast *right;
	struct ast *right2;
	struct ast *up;
	struct ast *next;
	struct ast *last;
	
	struct ast* typeAST;
	struct ast* param;
	struct ast* structStuff;
	
	union {
		int intVal;
		float floatVal;
		char* stringVal;
		long long int superIntVal;
	} value;
	
	int storage;
	int type;
	int sign;
	int qual;
	int scope;
	
	char* name;
	char* filename;
	int linenum;
	
	symrec* variable;
	
	int funVarOp;
	int offset;
	int size;
};

typedef struct ast ast;

ast* ast_new(char* ast_name, int ast_type) {
	ast *newOne = calloc(1, sizeof(ast));
	newOne->name = strdup(ast_name);
	newOne->type = ast_type;
	newOne->left = NULL;
	newOne->right = NULL;
	newOne->right2 = NULL;
	newOne->up = NULL;
	newOne->next = NULL;
	newOne->last = NULL;
	newOne->variable = NULL;
	newOne->filename = NULL;
	newOne->typeAST = NULL;
	
	newOne->value.superIntVal = 0;
	newOne->sign = 0;
	newOne->qual = 0;
	newOne->storage = 0;
	newOne->size = 4;
	
	return newOne;
}

ast* add_node(ast** begin, char* ast_name, int ast_type) {
	if (*begin == NULL) {
		*begin = ast_new(ast_name, ast_type);
		return *begin;
	}
	
	ast* temp = *begin;
	while(temp->left != NULL) {
		temp = temp->left;
	}
	
	temp->left = ast_new(ast_name, ast_type);
	temp->left->up = temp;
	
	return temp->left;
}

void connectHor(ast* begin, ast* end) {
	ast* temp = begin;
	while(temp->next != NULL) {
		temp = temp->next;
	}
	
	temp->next = end;
	end->last = temp;
}

void connectDown(ast** begin, ast* end) {
	ast* temp = *begin;
	
	if (temp == NULL) {
		*begin = end;
		return;
	}
	
	while(temp->left != NULL) {
		temp = temp->left;
	}
	
	temp->left = end;
	end->up = temp;
}

ast* clone(ast* original) {
	ast *newOne = calloc(1, sizeof(ast));
	newOne->name = strdup(original->name);
	newOne->type = original->type;
	newOne->storage = original->storage;
	newOne->left = original->left;
	newOne->right = original->right;
	newOne->right2 = original->right2;
	newOne->up = original->up;
	newOne->next = original->next;
	newOne->last = original->last;
	
	return newOne;
}

void insertVertical(ast* *newOne, ast* *oldOne) {
	(*newOne)->left = (*oldOne);
	(*newOne)->right = NULL;
	(*newOne)->right2 = NULL;
	
	
	if((*oldOne)->up != NULL && (*oldOne)->up->left == (*oldOne))
		(*oldOne)->up->left = (*newOne);
	
	if((*oldOne)->up != NULL && (*oldOne)->up->right == (*oldOne))
		(*oldOne)->up->right = (*newOne);
		
	if((*oldOne)->up != NULL && (*oldOne)->up->right2 == (*oldOne))
		(*oldOne)->up->right2 = (*newOne);
	
	(*newOne)->next = (*oldOne)->next;
	if ((*newOne)->next != NULL)
		(*newOne)->next->last = (*newOne);
	
	(*newOne)->last = (*oldOne)->last;
	if ((*newOne)->last != NULL)
		(*newOne)->last->next = (*newOne);
	
	(*newOne)->up = (*oldOne)->up;
	(*oldOne)->up = (*newOne);
	
	
}

#endif
