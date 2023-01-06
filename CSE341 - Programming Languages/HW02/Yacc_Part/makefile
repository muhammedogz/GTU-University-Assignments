
compile: gpp_lexer.l gpp_interpreter.y
	yacc -d -o gpp_interpreter.c gpp_interpreter.y
	flex -o gpp_lexer.c gpp_lexer.l 
	gcc gpp_lexer.c gpp_interpreter.c -o gpp_interpreter -ll -w

run: 
	./gpp_interpreter

clean:
	rm gpp_interpreter gpp_interpreter.c gpp_lexer.c