# FLEX Compiler
FLEX = flex
# YACC Compiler
YACC = yacc
# C Compiler
COMPILER = gcc

# prevent to yywrap usage
GCC_FLAG = -ll

# YACC Helper Header
YACC_HELPER = yacc_helper.h

# Lexer file
LEXER = gpp_lexer
# Interpreter file
INTERPRETER = gpp_interpreter

# Output file
OUTPUT = gpp_interpreter

# Test Input File
INPUTFILE = example_input.txt
# Test Output File
OUTPUTFILE = example_output.txt

flex: ${LEXER}.l ${INTERPRETER}.y ${YACC_HELPER}
	${YACC} -d -o ${INTERPRETER}.c ${INTERPRETER}.y
	${FLEX} -o ${LEXER}.c $(LEXER).l 
	${COMPILER} ${LEXER}.c ${INTERPRETER}.c  -o ${OUTPUT} ${GCC_FLAG}
	./${OUTPUT}

input:
	./${FILE} ${INPUTFILE}

output:
	./${FILE} ${INPUTFILE} > ${OUTPUTFILE}
