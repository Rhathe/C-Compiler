#ifndef _TARGET_H
#define _TARGET_H

#include "stdio.h"
#include "stdlib.h"
#include "string.h"

int max;
int maxparam;
int strLabel;
char str[1000];
char holdon[1000];
int bOffset;
int sOffset;
char curFunc[1000];

int getdelim(char* s) {
	int i;
	for (i = 0; i < strlen(s); ++i) {
		switch(s[i]) {
			case '{':
			case ' ': return i;
		}
	}
	return -1;
}

int getNum(char* s) {
	int delim = getdelim(s);
	if (delim == -1) {
		return -1;
	}
	else if (s[delim] == '{') {
		int k = strtol((s+delim+1), NULL, 10);
		if (k < max) max = k;
		return k;
	}
}

void dest() {
	if (bOffset != -1)
		fprintf(tempfile, "%d(%%ebp)\n", bOffset);
	else {
		if (!strcmp(holdon, "%RET"))
			fprintf(tempfile, "%%eax\n");
		else 
			fprintf(tempfile, "_%s\n", holdon);
	}
}

void takeAction(char *s) {
	char temp[1000];
	
	if (!strcmp(s, "MOV.4")) {
		fprintf(tempfile, "\tmovl \t");
		
		fscanf(myfile, "%s", temp);
		if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		dest();
	}
	
	else if (!strcmp(s, "LEA")) {
		fprintf(tempfile, "\tleal \t");
		fscanf(myfile, "%s", temp);
		if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		fprintf(tempfile, "%%eax\n");
		fprintf(tempfile, "\tmovl \t%%eax, ");
		fprintf(tempfile, "%d(%%ebp)\n", bOffset);
	}
	
	else if (!strcmp(s, "INC") || !strcmp(s, "DEC")) {
		if (!strcmp(s, "INC"))
			fprintf(tempfile, "\taddl \t");
		else 
			fprintf(tempfile, "\tsubl \t");
		
		fscanf(myfile, "%s", temp);
		temp[strlen(temp)-1] = '\0';
		
		fscanf(myfile, "%s", s);
		fprintf(tempfile, "%s, ", s);
		
		if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp)\n", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s\n", temp);
		}
	}
	
	else if (!strcmp(s, "ADD.4") ||
			!strcmp(s, "SUBL.4") ||
			!strcmp(s, "MUL.4") ||
			!strcmp(s, "DIV.4") ||
			!strcmp(s, "AND") ||
			!strcmp(s, "OR") ||
			!strcmp(s, "XOR") ||
			!strcmp(s, "SHR") ||
			!strcmp(s, "SHL"))
	{
		fprintf(tempfile, "\tmovl \t");
		fscanf(myfile, "%s", temp);
		temp[strlen(temp)-1] = '\0';
		if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		fprintf(tempfile, "%%eax\n");
		////////////////////////
		
		if (!strcmp(s, "ADD.4"))
			fprintf(tempfile, "\taddl \t");
		else if (!strcmp(s, "MUL.4"))
			fprintf(tempfile, "\timull \t");
		else if (!strcmp(s, "SUB.4"))
			fprintf(tempfile, "\tsubl \t");
		else if (!strcmp(s, "DIV.4"))
			fprintf(tempfile, "\tidivl \t");
		else if (!strcmp(s, "AND"))
			fprintf(tempfile, "\tandl \t");
		else if (!strcmp(s, "OR"))
			fprintf(tempfile, "\torl \t");
		else if (!strcmp(s, "XOR"))
			fprintf(tempfile, "\txorl \t");
		else if (!strcmp(s, "SHR"))
			fprintf(tempfile, "\tsar \t");
		else if (!strcmp(s, "SHL"))
			fprintf(tempfile, "\tsal \t");
		
		fscanf(myfile, "%s", temp);
		if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		fprintf(tempfile, "%%eax\n");
		
		fprintf(tempfile, "\tmovl \t%%eax, ");
		dest();
	}
	
	else if (!strcmp(s, "NOT")) {
		fscanf(myfile, "%s", temp);
		fprintf(tempfile, "\tnotl \t");
		
		if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		dest();
	}
	
	else if (!strcmp(s, "FLIP")) {
		fscanf(myfile, "%s", temp);
		fprintf(tempfile, "\tmovl \t");
		
		if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		
		fprintf(tempfile, "%%eax\n");
		fprintf(tempfile, "\txorl \t$0, %%eax\n");
		fprintf(tempfile, "\tmovl \t%%eax, ");
		dest();
	}
	
	else if (!strcmp(s, "STORE.4")) {
		fscanf(myfile, "%s", temp);
		temp[strlen(temp)-1] = '\0';
		
		fprintf(tempfile, "\tmovl \t");
		fscanf(myfile, "%s", s);
		if (s[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (s[strlen(s)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(s));
		}
		else {
			fprintf(tempfile, "_%s, ", s);
		}
		fprintf(tempfile, "%%eax\n");
		
		fprintf(tempfile, "\tmovl \t");
		if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		fprintf(tempfile, "(%%eax)\n");
	}
	
	else if (!strcmp(s, "LOAD.4")) {
		fprintf(tempfile, "\tmovl \t");
		fscanf(myfile, "%s", temp);
		
		if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		fprintf(tempfile, "%%eax\n");
		
		fprintf(tempfile, "\tmovl \t(%%eax), %%ecx\n");
		fprintf(tempfile, "\tmovl \t%%ecx, ");
		dest();
	}
	
	else if (!strcmp(s, "MOVSTR")) {
		fgets(temp, 1000, myfile);
		fprintf(sfile, "\t.section \t.rodata\n.LC%d:\n", strLabel);
		fprintf(sfile, "\t.string %s\t.text\n", temp);
		
		fprintf(tempfile, "\tmovl \t$.LC%d, ", strLabel);
		dest();
		strLabel++;
	}
	
	else if (!strcmp(s, "BRGE") ||
			!strcmp(s, "BRLE") ||
			!strcmp(s, "BREE") ||
			!strcmp(s, "BRNE") ||
			!strcmp(s, "BRG") ||
			!strcmp(s, "BRL") ||
			!strcmp(s, "CMP.4") ||
			!strcmp(s, "GOTO"))
	{
		if (!strcmp(s, "BRGE"))
			fprintf(tempfile, "\tjge \t");
		else if (!strcmp(s, "BRLE"))
			fprintf(tempfile, "\tjle \t");
		else if (!strcmp(s, "BREE"))
			fprintf(tempfile, "\tje \t");
		else if (!strcmp(s, "BRNE"))
			fprintf(tempfile, "\tjne \t");
		else if (!strcmp(s, "BRG"))
			fprintf(tempfile, "\tjg \t");
		else if (!strcmp(s, "BRL"))
			fprintf(tempfile, "\tjl \t");
		else if (!strcmp(s, "GOTO"))
			fprintf(tempfile, "\tjmp \t");
		else if (!strcmp(s, "CMP.4")) {
			fscanf(myfile, "%s", temp);
			fprintf(tempfile, "\tmovl \t");
			temp[strlen(temp)-1] = '\0';
			
			if (temp[0] == '$') {
				fprintf(tempfile, "%s, ", temp);
			}
			else if (temp[strlen(temp)-1] == '}'){
				fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
			}
			else {
				fprintf(tempfile, "_%s, ", temp);
			}
			fprintf(tempfile, "%%eax\n");
			
			fscanf(myfile, "%s", temp);
			fprintf(tempfile, "\tmovl \t");
			if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
			}
			else if (temp[strlen(temp)-1] == '}'){
				fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
			}
			else {
				fprintf(tempfile, "_%s, ", temp);
			}
			fprintf(tempfile, "%%ecx\n");
			
			fprintf(tempfile, "\tcmpl \t%%ecx, %%eax\n");
			return;
		}
		
		fscanf(myfile, "%s", temp);
		
		if (strcmp(s, "GOTO"))
			temp[strlen(temp)-1] = '\0';
		
		if (strcmp(s, "CMP.4")) {
			fprintf(tempfile, "%s\n", temp);
			
			if (strcmp(s, "GOTO"))
				fscanf(myfile, "%s", temp);
		}
	}
	
	else if (!strcmp(s, "ARG.4")) {
		fscanf(myfile, "%s", temp);
		int x = strtol(temp+1, NULL, 10);
		int y = (x/4+1)*4;
		if (y > maxparam) maxparam = y;
		
		fprintf(tempfile, "\tmovl \t");
		fscanf(myfile, "%s", temp);
		
		if (temp[0] == '$') {
			fprintf(tempfile, "%s, ", temp);
		}
		else if (temp[strlen(temp)-1] == '}'){
			fprintf(tempfile, "%d(%%ebp), ", getNum(temp));
		}
		else {
			fprintf(tempfile, "_%s, ", temp);
		}
		fprintf(tempfile, "%%eax\n", temp);
		
		fprintf(tempfile, "\tmovl \t%%eax, ", temp);
		if (x != 0)
			fprintf(tempfile, "%d(%%esp)\n", x);
		else 
			fprintf(tempfile, "(%%esp)\n");
	}
	
	else if (!strcmp(s, "CALL")) {
		fscanf(myfile, "%s", temp);
		temp[strlen(temp)-1] = '\0';
		
		fprintf(tempfile, "\tcall \t_%s\n", temp);
		fscanf(myfile, "%s", temp);
		
		fprintf(tempfile, "\tmovl \t%%eax, ");
		dest();
	}
}

void generate() {
	myfile = fopen ("myfile","r");
	tempfile = fopen ("tempfile","w");
	sfile = fopen ("out.s","w");
	max = 0;
	maxparam = 0;
	strLabel = 0;
	
	while (!feof(myfile)) {
		fscanf(myfile, "%s", str);
		
		if (str[strlen(str)-1] == ':') {
			if (str[0] != '.') {
				max = 0;
				maxparam =  0;
				str[strlen(str)-1] = '\0';
				strcpy(curFunc, str);
				fprintf(tempfile, ".globl _%s\n", str, str);
				str[strlen(str)] = ':';
				fprintf(tempfile, "_%s\n",str);
				fprintf(tempfile, "\tpushl \t%%ebp\n");
				fprintf(tempfile, "\tmovl \t%%esp,%%ebp\n");
				fprintf(tempfile, "\t$REPLACE\n");
				
				if (!strcmp(curFunc, "main")) {
					fprintf(tempfile, "\tmovl \t$0, %%eax\n");
					
					//fprintf(tempfile, "\tcall \t__alloca\n");
					fprintf(tempfile, "\tcall \t___main\n");
				}
			}
			else {
				fprintf(tempfile, "%s\n",str);
			}
		}
		else if (!strcmp(str, "RETURN")) {
			fclose(tempfile);
			tempfile = fopen ("tempfile","r");
			
			char endoffile[1000];
			while (!feof(tempfile)) {
				strcpy(endoffile, str);
				fgets(str, 1000, tempfile);
				if(strcmp(endoffile,str)) {
					if (!strcmp(str, "\t$REPLACE\n")) {
						max -= maxparam;
						while((max % 16)) {
							max -= 4;
						}
						max -= 4;
						fprintf(sfile, "\tsubl \t$%d, %%esp\n", -max);
					}
					else fprintf(sfile, "%s", str);
				}
			}
			
			fprintf(sfile, "\tleave\n\tret\n\n");
			
			fclose(tempfile);
			tempfile = fopen ("tempfile","w");
		}
		else if (!strcmp(str, "STORE.4") ||
				!strcmp(str, "ARG.4") ||
				!strcmp(str, "BRGE") ||
				!strcmp(str, "BRLE") ||
				!strcmp(str, "BREE") ||
				!strcmp(str, "BRNE") ||
				!strcmp(str, "BRG") ||
				!strcmp(str, "BRL") ||
				!strcmp(str, "GOTO") ||
				!strcmp(str, "CMP.4"))
		{
			takeAction(str);
		}
		else {
			//fprintf(tempfile, "%s\n", str);
			//fgets(str, 20, myfile);
			//str[strlen(str)-1] = '\0';
			bOffset = getNum(str);
			if (bOffset != -1) {
				//fprintf(tempfile, "%d\n", bOffset);
				fscanf(myfile, "%s", str);
				fscanf(myfile, "%s", str);
				takeAction(str);
			}
			else {
				strcpy(holdon, str);
				fscanf(myfile, "%s", str);
				fscanf(myfile, "%s", str);
				takeAction(str);
			}
		}
	}
	
	fclose(commfile);
	commfile = fopen ("commfile","r");
	
	while(1) {
		fgets(str, 1000, commfile);
		if (feof(commfile)) return;
		
		fprintf(sfile, "%s", str);
	}
}

#endif