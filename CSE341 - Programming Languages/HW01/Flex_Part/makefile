FLEXFILE = gpp_lexer.l;
INPUTFILE = example_input.txt;

flex: $(FLEXFILE)
	flex $(FLEXFILE)
	gcc lex.yy.c
	./a.out

read:
	./a.out ${INPUTFILE}
