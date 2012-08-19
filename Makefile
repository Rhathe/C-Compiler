grammar: lex.yy.c grammar.tab.c
	gcc lex.yy.c grammar.tab.c -g -o grammar

lex.yy.c: lexer.l
	flex lexer.l

grammar.tab.c: grammar.y calc.h calcfnc.h ast.h global.h ir.h target.h
	bison -d -v grammar.y

test:
	gcc -E test.c | ./grammar

clean:
	rm lex.yy.c
	rm y.tab.c
	rm grammar
	rm grammar.exe
	rm *.exe *.out *.o *.stackdump *~
