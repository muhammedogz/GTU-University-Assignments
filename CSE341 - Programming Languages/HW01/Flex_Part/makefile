FLEX = flex
COMPILER = gcc
FILE = gpp_lexer

INPUTFILE = example_input.txt

flex: ${FILE}.l
	${FLEX} -o ${FILE}.c $(FILE).l
	${COMPILER} ${FILE}.c -o ${FILE}
	./${FILE}

read:
	./${FILE} ${INPUTFILE}
