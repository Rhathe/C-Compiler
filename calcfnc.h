void freedom(symrec *sym_table) {
	symrec *ptr, *temp;
	ptr = sym_table;
	int i;
	for (i = 0; i < NHASH; ++i) {
		if (ptr[i].name != NULL) {
			free(ptr[i].name);
		}	
	}
	free(sym_table);
}

void printOut (symrec *sym_table, char *sym_name) {
  symrec *ptr;
  for (ptr = sym_table; ptr != (symrec *) 0;
       ptr = (symrec *)ptr->next)
    { printf("%s\n",ptr->name);}
}

symrec *symtable() {

	symrec* table = (symrec*) calloc(NHASH, sizeof(symrec));
	
	return table;
}

unsigned int symhash(char *sym) {
	unsigned int hash = 0;
	int c;
	
	while (c = *sym++) 
		hash = hash*9 ^c;
	
	return hash%NHASH;
}

symrec* insert(symrec *in_table, char *sym_name, int sym_type) {
	int slot = symhash(sym_name);
	in_table[slot].name = strdup(sym_name);
	in_table[slot].type = sym_type;
	in_table[slot].value.var = 0;
	
	return in_table + slot;
}

symrec *lookup(symrec* sym_table, char* sym) {
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
