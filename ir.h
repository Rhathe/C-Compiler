#ifndef IR_H
#define IR_H

#include "ast.h"
#include "grammar.tab.h"
#include "global.h"

ast* gen_rvalue(ast* node, ast* target);
ast* gen_lvalue(ast* node, int* mode);
ast* gen_assign(ast* node);
int gen_cond(ast* node, ast* begin, ast* end);
void irTree(ast* node, ast* begin, ast* end);
void branch(int type, int invert, char *s1, char *s2);

int binop(int op) {
	int c;
	switch(op) {
		case '=': return op;
		case '+':
		case '-':
		case '/':
		case '*':
		case '%':
		case SHL:
		case SHR:
		case '&':
		case '^':
		case '|':
		case LOGAND:
		case LOGOR:
		case ',': 
		case '!':
		case '~': 
		case PLUSPLUS:
		case MINUSMINUS: 
		case MINUS: return 2;
		case '<':
		case '>':
		case LTEQ:
		case GTEQ:
		case EQEQ:
		case NOTEQ: return 3;
		default: return 0;
	}
}

int emit(int opcode, ast* left, ast* right, ast* target) {
	if (target != NULL) {
		if (target->name[0] == '%') fprintf(myfile, "\t%s = ", target->name);
		else if (target->offset == 0) fprintf(myfile, "\t%s = ", target->name);
		else fprintf(myfile, "\t%s{%d} = ", target->name,target->offset);
	}
	switch(opcode) {
		case '+':		fprintf(myfile, "\tADD.4 \t");
						break;
		case MINUS:
		case '*':		fprintf(myfile, "\tMUL.4 \t");
						break;
		case '-':		fprintf(myfile, "\tSUB.4 \t");
						break;
		case '/':		fprintf(myfile, "\tDIV.4 \t");
						break;
		case '%':		fprintf(myfile, "\tMOD.4 \t");
						break;
		case ',':		right = NULL;
						break;
		case '[':		fprintf(myfile, "\tLEA.4 \t");
						break;
		case LOAD:	fprintf(myfile, "\tLOAD.4 \t");
						break;
		case LEA:		fprintf(myfile, "\tLEA \t");
						break;
		case STORE:	fprintf(myfile, "\tSTORE.4 \t");
						break;
		case CMP:	fprintf(myfile, "\tCMP.4 \t");
						break;
		case MOV:	fprintf(myfile, "\tMOV.4 \t");
						break;
		case PASSARG:	fprintf(myfile, "\tARG.4 \t");
						break;
		case CALL:	fprintf(myfile, "\tCALL \t");
						break;
		case PLUSPLUS:	fprintf(myfile, "\tINC \t");
						break;
		case MINUSMINUS:	fprintf(myfile, "\tDEC \t");
						break;
		case '~':	fprintf(myfile, "\tFLIP \t");
						break;
		case '!':	fprintf(myfile, "\tNOT \t");
						break;
		case SHL:	fprintf(myfile, "\tSHL \t");
						break;
		case SHR:	fprintf(myfile, "\tSHR \t");
						break;
		case LOGAND:	fprintf(myfile, "\tLOGAND \t");
						break;
		case LOGOR:	fprintf(myfile, "\tLOGOR \t");
						break;
		case '&':	fprintf(myfile, "\tAND \t");
						break;
		case '^':	fprintf(myfile, "\tXOR \t");
						break;
		case '|':	fprintf(myfile, "\tOR \t");
						break;
		case MOVSTR:	fprintf(myfile, "\tMOVSTR \"%s\\0\"\n", left->name);
						return 0;
	}
	
	if (left != NULL) 
	{
		if (left->type == STRING) { 
			fprintf(myfile, "\"%s\"", left->name);
		}
		else if (left->type != NUMBER) {
			if (left->name[0] == '%') fprintf(myfile, "%s", left->name);
			else if (left->typeAST != NULL) {
				if (left->typeAST->type == FUNCTION) fprintf(myfile, "%s", left->name);
				else if (left->offset == 0)
					fprintf(myfile, "%s", left->name);
				else
					fprintf(myfile, "%s{%d}", left->name, left->offset);
			}
			else if (left->offset == 0)
				fprintf(myfile, "%s", left->name);
			else
				fprintf(myfile, "%s{%d}", left->name, left->offset);
		}
		else fprintf(myfile, "$%d", left->value.intVal);
	}
	else {
		fprintf(myfile, "blah");
	}
	
	if (right != NULL) 
	{
		if (right->type == STRING) { 
			fprintf(myfile, ", \"%s\"", right->name);
		}
		else if (right->type != NUMBER) {
			if (right->name[0] == '%') fprintf(myfile, ", %s", right->name);
			else if (right->typeAST != NULL) {
				if (right->typeAST->type == FUNCTION) fprintf(myfile, ", %s", right->name);
				else if (right->offset == 0) fprintf(myfile, ", %s", right->name);
				else fprintf(myfile, ", %s{%d}", right->name, right->offset);
			}
			else if (right->offset == 0) fprintf(myfile, ", %s", right->name);
			else fprintf(myfile, ", %s{%d}", right->name, right->offset);
		}
		else fprintf(myfile, ", $%d", right->value.intVal);
	}
	
	fprintf(myfile, "\n");
}

ast* new_temp() {
	int indices = 10;
	char s[indices];
	int i = 0;
	for (i = 0; i < indices; ++ i)
		s[i] = 0;
	
	sprintf(s, "%%T{%d}", localoffset-tempNum*4);
	ast* temp = ast_new(s, TEMPORARY);
	++tempNum;
	return temp;
}

ast* new_block() {
	int indices = 10;
	char s[indices];
	int i = 0;
	for (i = 0; i < indices; ++ i)
		s[i] = 0;
	
	++blockNum;
	sprintf(s, ".BB%d.%d", fncNum, blockNum);
	ast* block = ast_new(s, TEMPORARY);
	return block;
}

ast* ptMath(ast* node, ast* target, int nodetype) {
		ast* left = gen_rvalue(node->left, NULL);
		ast* right = gen_rvalue(node->right, NULL);
		ast* tarleft = left;
		ast* tarright = right;
		int tartype = TEMPORARY;
		
		ast num1;
		num1.type = NUMBER;
		num1.value.intVal = 4;
		
		
		if (left != NULL && right != NULL) {
			
			if (left->typeAST != NULL && left->typeAST->type == POINTER) {
				tarleft = new_temp();
				emit(LEA,left,NULL,tarleft);
				tarleft->type = ADDRVAL;
				tartype = ADDRVAL;
			}
			
			if (right->typeAST != NULL && right->typeAST->type == POINTER)
			{
				tarright = new_temp();
				emit(LEA,right,NULL,tarright);
				tarright->type = ADDRVAL;
				tartype = ADDRVAL;
			}
		}
		
		if (tarleft->type == ADDRVAL) {
			if (tarright != NULL && tarright->type != ADDRVAL) {
				right = new_temp();
				emit('*',tarright,&num1,right);
				left = tarleft;
			}
			else {
				left = tarleft;
				right = tarright;
				tartype = TEMPORARY;
				ast* finaltemp = new_temp();
				emit(nodetype, left, right, finaltemp);
				nodetype = '/';
				left = finaltemp;
				right = &num1;
			}
		}
		else if (tarright != NULL && tarright->type == ADDRVAL) {
			left = new_temp();
			emit('*',tarleft,&num1,left);
			right = tarright;
		}
		else if (tarright == NULL) {
			if (node->type == PLUSPLUS || node->type == MINUSMINUS) {
				if (tarleft->type == ADDRVAL)
					right = &num1;
				else {
					num1.value.intVal = 1;
					right = &num1;
				}
			}
			else if (node->type == MINUS) {
				num1.value.intVal = -1;
				right = &num1;
			}
		}
		
		if (target == NULL) target = new_temp();
		target->type = tartype;
		emit(nodetype, left, right, target);
		return target;

}

int isMember(ast *var, ast *mem, int type) {
	ast* typeAST;
	
	if (type == '.')
		typeAST = var->typeAST->structStuff;
	else
		typeAST = var->typeAST->left->structStuff;
	
	while(typeAST != NULL) {
		if (!strcmp(typeAST->name, mem->name))
			return typeAST->offset;
		typeAST = typeAST->next;
	}
	
	return -1;
}

ast* member(ast* node) {
	ast *left, *right, *target;
	ast *newleft, *newright;
	left = gen_rvalue(node->left, NULL);
	if (node->right->type == '.' || node->right->type == INDSEL)
		right = member(node->right);
	else
		right = node->right;
	
	int offset = isMember(left, right, node->type);
	
	if (offset == -1) {
		fprintf(stderr, "ERROR: %s not a member of %s\n", right->name, left->name);
		return NULL;
	}
	
	if (node->type == '.') {
		if (left->typeAST->type == UNION)
			return left;
	}
	else if (node->type == INDSEL) {
		if (left->typeAST->left->type == UNION) {
			target = new_temp();
			emit(LOAD, left, NULL, target);
			target->type = ADDRVAL;
			return target;
		}
	}
	
	newleft = left;
	newright = right;
	
	if (left->type != ADDRVAL) {
		newleft = new_temp();
		emit(LEA, left, NULL, newleft);
	}
	
	ast num1;
	num1.type = NUMBER;
	num1.value.intVal = offset;
	
	target = new_temp();
	emit('+', newleft, &num1, target);
	target->typeAST = right->typeAST;
	target->type = ADDRVAL;
	return target;
}

ast* call(ast* node, ast* target) {

	ast num1;
	num1.type = NUMBER;
	num1.value.intVal = 0;
	
	ast *right = node->right;
	
	while (right != NULL) {
		ast *temp = gen_rvalue(right, NULL);
		num1.value.intVal = num1.value.intVal*4;
		emit(PASSARG, &num1, temp, NULL);
		num1.value.intVal = num1.value.intVal/4;
		++num1.value.intVal;
		right = right->next;
	}
	
	if (target == NULL) target = new_temp();
	emit(CALL, node->left, &num1, target);
	return target;
}

ast* gen_rvalue(ast* node, ast* target) {
	if (node == NULL) return NULL;

	if (node->type == VARIABLE) return node;
	if (node->type == NUMBER) return node;
	if (node->type == STRING) {
		if (target == NULL) target = new_temp();
		emit(MOVSTR, node, NULL, target);
		return target;
	}
	if (node->type == CHARLIT) return node;
	if (node->type == '=') return gen_assign(node);
	if (node->type == ADDRESS) {
		ast* addr = gen_rvalue(node->left, NULL);
		if (target == NULL) target = new_temp();
		emit(LEA, addr, NULL, target);
		if (addr != NULL) target->typeAST = addr->typeAST;
		return target;
	}
	if (node->type == DEREF) {
		ast* addr = gen_rvalue(node->left, NULL);
		if (target == NULL) target = new_temp();
		emit(LOAD, addr, NULL, target);
		if (addr != NULL) target->typeAST = addr->typeAST;
		return target;
	}
	if (node->type == '[') {
		ast* addr = ptMath(node, NULL, '+');
		if (target == NULL) target = new_temp();
		emit(LOAD, addr, NULL, target);
		if (addr != NULL) target->typeAST = addr->typeAST;
		return target;
	}
	if (node->type == '.' || node->type == INDSEL) {
		ast* addr = member(node);
		if (target == NULL) target = new_temp();
		emit(LOAD, addr, NULL, target);
		if (addr != NULL) target->typeAST = addr->typeAST;
		return target;
	}
	if (node->type == LOGAND) {
		ast num1;
		num1.type = NUMBER;
		num1.value.intVal = 0;
		
		ast* left = gen_rvalue(node->left, NULL);
		ast* right = gen_rvalue(node->right, NULL);
		ast* b1 = new_block();
		ast* b2 = new_block();
		
		if (target == NULL) target = new_temp();
		
		emit(CMP, &num1, left, NULL);
		fprintf(myfile, "\tBREE %s, %s\n",b1->name, "blah");
		
		emit(CMP, &num1, right, NULL);
		fprintf(myfile, "\tBREE %s, %s\n",b1->name, "blah");
		
		num1.value.intVal = 1;
		emit(MOV, &num1, NULL, target);
		fprintf(myfile, "\tGOTO %s\n",b2->name);
		
		fprintf(myfile, "%s:\n", b1->name);
		num1.value.intVal = 0;
		emit(MOV, &num1, NULL, target);
		fprintf(myfile, "%s:\n", b2->name);
		
		return target;
	}
	if (node->type == LOGOR) {
		ast num1;
		num1.type = NUMBER;
		num1.value.intVal = 0;
		
		ast* left = gen_rvalue(node->left, NULL);
		ast* right = gen_rvalue(node->right, NULL);
		ast* b1 = new_block();
		ast* b2 = new_block();
		ast* b3 = new_block();
		
		if (target == NULL) target = new_temp();
		
		emit(CMP, &num1, left, NULL);
		fprintf(myfile, "\tBRNE %s, %s\n",b1->name, "blah");
		
		emit(CMP, &num1, right, NULL);
		fprintf(myfile, "\tBREE %s, %s\n",b2->name, "blah");
		
		fprintf(myfile, "%s:\n", b1->name);
		num1.value.intVal = 1;
		emit(MOV, &num1, NULL, target);
		fprintf(myfile, "\tGOTO %s\n",b3->name);
		
		fprintf(myfile, "%s:\n", b2->name);
		num1.value.intVal = 0;
		emit(MOV, &num1, NULL, target);
		fprintf(myfile, "%s:\n", b3->name);
		
		return target;
	}
	else if (binop(node->type) == 3){
		ast* left = gen_rvalue(node->left, NULL);
		ast* right = gen_rvalue(node->right, NULL);
		
		ast* b1 = new_block();
		ast* b2 = new_block();
		
		ast num1;
		num1.type = NUMBER;
		num1.value.intVal = 0;
		
		if (target == NULL) target = new_temp();
		
		emit(CMP, left, right, NULL);
		branch(node->type, 0, b1->name, "blah");
		
		emit(MOV, &num1, NULL, target);
		fprintf(myfile, "\tGOTO %s\n", b2->name);
		fprintf(myfile, "%s:\n", b1->name);
		
		num1.value.intVal = 1;
		emit(MOV, &num1, NULL, target);
		
		fprintf(myfile, "%s:\n", b2->name);
		
		return target;
	}
	else if (binop(node->type) == 2){
		return ptMath(node, target, node->type);
	}
	else if (node->type == '(') {
		ast* ret = call(node, target);
		if (target == NULL)
			return ret;
		else
			return NULL;
	}
	else if (node->type == ARG){
		return gen_rvalue(node->left, NULL);
	}
	else
		return NULL;
}

ast* gen_lvalue(ast* node, int* mode) {
	if (node == NULL) return NULL;
	
	else if (node->type == VARIABLE) {*mode = DIRECT; return node;}
	else if (node->type == NUMBER) return NULL;
	else if (node->type == STRING) return NULL;
	else if (node->type == CHARLIT) return NULL;
	else if (node->type == DEREF) {
		*mode = INDIRECT;
		return gen_rvalue(node->left, NULL);
	}
	else if (node->type == '[') {
		*mode = INDIRECT;
		return ptMath(node, NULL, '+');
	}
	else if (node->type == '.' || node->type == INDSEL) {
		*mode = INDIRECT;
		return member(node);
	}
	else {
		return gen_lvalue(node->left, mode);
	}
}

ast* gen_assign(ast* node) {
	int dstmode;
	if (node == NULL) return NULL;
	ast* dst = gen_lvalue(node->left, &dstmode);
	if (dst == NULL) return NULL;
	
	if (node->type != '=') {
		return gen_assign(node->left);
	}
	
	if (dstmode == DIRECT) {
		ast* right = gen_rvalue(node->right, dst);
		if (right != NULL && (right->type == NUMBER || right->type == VARIABLE)) {
			if (right != dst)
				emit(MOV, right, NULL, dst);
		}
		else if (right != NULL && right->type == STRING) {
			;//emit(MOV, right, NULL, dst);
		}
		return dst;
	}
	else {
		ast* left = gen_rvalue(node->right, NULL);
		emit(STORE, left, dst, NULL);
		return dst;
	}
}

void branch(int type, int invert, char *s1, char *s2) {
	if (invert == 1) {
		switch(type) {
			case '<': fprintf(myfile, "\tBRGE %s, %s\n",s1, s2);
					return;
			case '>': fprintf(myfile, "\tBRLE %s, %s\n",s1, s2);
					return;
			case LTEQ: fprintf(myfile, "\tBRG %s, %s\n",s1, s2);
					return;
			case GTEQ: fprintf(myfile, "\tBRL %s, %s\n",s1, s2);
					return;
			case EQEQ: fprintf(myfile, "\tBRNE %s, %s\n",s1, s2);
					return;
			case NOTEQ: fprintf(myfile, "\tBREE %s, %s\n",s1, s2);
					return;
		}
	}
	else {
		switch(type) {
			case '<': fprintf(myfile, "\tBRL %s, %s\n",s1, s2);
					return;
			case '>': fprintf(myfile, "\tBRG %s, %s\n",s1, s2);
					return;
			case LTEQ: fprintf(myfile, "\tBRLE %s, %s\n",s1, s2);
					return;
			case GTEQ: fprintf(myfile, "\tBRGE %s, %s\n",s1, s2);
					return;
			case EQEQ: fprintf(myfile, "\tBREE %s, %s\n",s1, s2);
					return;
			case NOTEQ: fprintf(myfile, "\tBRNE %s, %s\n",s1, s2);
					return;
		}
	}
}

int gen_cond(ast* node, ast* begin, ast* end) {
	ast *yes, *no, *out;
	ast *left, *right, *right2;
	ast *target;
	
	if (node == NULL) return 0;
	else if (node->type == NUMBER) {
		ast num1;
		num1.type = NUMBER;
		num1.value.intVal = 0;
		emit(CMP, node, &num1, NULL);
		return NOTEQ;
	}
	else if (node->type == VARIABLE) {
		ast num1;
		num1.type = NUMBER;
		num1.value.intVal = 0;
		emit(CMP, node, &num1, NULL);
		return NOTEQ;
	}
	else if (node->type == IF) {
		no = new_block();
		yes = new_block();
		int type = gen_cond(node->left, begin, end);
		branch(type, 1, yes->name, no->name);
		fprintf(myfile, "%s:\n", no->name);
		irTree(node->right, begin, end);
		if (node->next == NULL || (node->next != NULL && node->next->type != ELSE))
			fprintf(myfile, "%s:\n", yes->name);
		else {
			out = new_block();
			fprintf(myfile, "\tGOTO %s\n", out->name);
			fprintf(myfile, "%s:\n", yes->name);
			irTree(node->next->left, begin, end);
			fprintf(myfile, "%s:\n", out->name);
		}
		return 0;
	}
	else if (binop(node->type) == 3) {
		left = gen_rvalue(node->left, NULL);
		right = gen_rvalue(node->right, NULL);
		emit(CMP, left, right, NULL);
		return node->type;
	}
	else if (binop(node->type) == 2) {
		ast* temp = gen_rvalue(node, NULL);
		ast num1;
		num1.type = NUMBER;
		num1.value.intVal = 0;
		emit(CMP, &num1, temp, NULL);
		return NOTEQ;
	}
	else return gen_cond(node->left, begin, end);
}

ast* gen_loop(ast* node) {
	ast *init, *cond, *body, *incr;
	ast *begin, *middle, *end;
	int type;
	
	if (node == NULL) return NULL;
	else if (node->type == FOR) {
		init = gen_assign(node->left);
		begin = new_block();
		middle = new_block();
		incr = new_block();
		end = new_block();
		
		fprintf(myfile, "%s:\n", begin->name);
		type = gen_cond(node->left->next, incr, end);
		branch(type, 1, end->name, middle->name);
		
		fprintf(myfile, "%s:\n", middle->name);
		irTree(node->left->next->next->next, incr, end);
		
		fprintf(myfile, "%s:\n", incr->name);
		irTree(node->left->next->next, incr, end);
		
		fprintf(myfile, "\tGOTO %s\n", begin->name);
		fprintf(myfile, "%s:\n", end->name);
		return NULL;
	}
	else if (node->type == WHILE) {
		begin = new_block();
		fprintf(myfile, "%s:\n", begin->name);
		middle = new_block();
		end = new_block();
		
		type = gen_cond(node->left, begin, end);
		branch(type, 1, end->name, middle->name);
		fprintf(myfile, "%s:\n", middle->name);
		irTree(node->left->next, begin, end);
		fprintf(myfile, "\tGOTO %s\n", begin->name);
		fprintf(myfile, "%s:\n", end->name);
		return NULL;
	}
	else if (node->type == DO) {
		begin = new_block();
		fprintf(myfile, "%s:\n", begin->name);
		end = new_block();
		irTree(node->left->next, begin, end);
		type = gen_cond(node->left, begin, end);
		branch(type, 0, begin->name, end->name);
		fprintf(myfile, "%s:\n", end->name);
		return NULL;
	}
}

void irTree(ast* myAST, ast* begin, ast* end) {
	if (myAST == NULL)
		return;
	
	if (binop(myAST->type) == '=') {
		gen_assign(myAST);
	}
	else if (myAST->type == PLUSPLUS || myAST->type == MINUSMINUS) {
		gen_rvalue(myAST, NULL);
	}
	else if (binop(myAST->type) == 2) {
		ast* left = gen_rvalue(myAST->left, NULL);
		ast* right = gen_rvalue(myAST->right, NULL);
	}
	else if (myAST->type == '(') {
		call(myAST, NULL);
	}
	else if (myAST->type == LABEL) {
		fprintf(myfile, ".BB%d.%s:\n", fncNum,myAST->name);
	}
	else if (myAST->type == IF) {
		gen_cond(myAST, begin, end);
	}
	else if (myAST->type == ELSE) ;
	else if (myAST->type == GOTO) {
		fprintf(myfile, "\tGOTO .BB%d.%s\n", fncNum,myAST->name);
	}
	else if (myAST->type == IDENT) {
		;
	}
	else if (myAST->type == FOR) {
		gen_loop(myAST);
	}
	else if (myAST->type == WHILE) {
		gen_loop(myAST);
	}
	else if (myAST->type == DO) {
		gen_loop(myAST);
	}
	else if (myAST->type == CONTINUE) {
		fprintf(myfile, "\tGOTO %s\n", begin->name);
	}
	else if (myAST->type == BREAK) {
		fprintf(myfile, "\tGOTO %s\n", end->name);
	}
	else if (myAST->type == RETURN) {
		if (myAST->left != NULL) {
			ast* ret = ast_new("%RET", TEMPORARY);
			emit(MOV, myAST->left, NULL, ret);
		}
		fprintf(myfile, "\tRETURN\n");
	}
	else if (myAST->type == INCR) {
		irTree(myAST->left, begin, end);
		return;
	}
	else {
		irTree(myAST->left, begin, end);
	}
	
	if (myAST->next != NULL) {
		irTree(myAST->next, begin, end);
	}
}

#endif
