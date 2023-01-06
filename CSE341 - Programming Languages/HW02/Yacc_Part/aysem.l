/* created to detect is " opeing or closing quotes?*/
%{
  #include <stdio.h>
    #include <stdlib.h>

    #include "gpp_interpreter.h" // interpreter generates this file
    int is_op_cc = 0;
%}

/* define the rules for the lexer by using regex*/
COMMENT [;]{2}.*
VALUE [1-9][0-9]*|[0]
IDENTIFIER  [a-zA-Z][a-zA-Z0-9]*




/* RULES*/

%%

    /* KEYWORD DEFINITIONS */

true    { return KW_TRUE;}
false   { return KW_FALSE;}
and     { return KW_AND;}
or      { return KW_OR;}
not     { return KW_NOT;}
equal   { return KW_EQUAL;} 
less    { return KW_LESS;}
nil     { return KW_NIL;}
list    { return KW_LIST;}
append  { return KW_APPEND;}
concat  { return KW_CONCAT;}
set     { return KW_SET;}
deffun  { return KW_DEFFUN;}
for     { return KW_FOR;}
if      { return KW_IF;}
load    { return KW_LOAD;}
disp    { return KW_DISP;}
exit    { return KW_EXIT;}

    /* OPERATOR DEFINITIONS */

[+]       { return OP_PLUS;}
[-]       { return OP_MINUS;}
[/]       { return OP_DIV;}
[*]       { return OP_MULT;}
[(]       { return OP_OP;}
[)]       { return OP_CP;} 
[']		    { return CUT;}

[*]{2}    { return OP_DBLMULT;}
[,]       { return OP_COMMA;}
["]       {if (is_op_cc == 0) { return OP_OC; is_op_cc = 1; } else { return OP_CC; is_op_cc = 0; }} 

{COMMENT} { return COMMENT;}
{IDENTIFIER}    { yylval.string = strdup(yytext); return IDENTIFIER; }
{VALUE}         { yylval.value = atoi(yytext); return VALUE; }

[ \t\n]  {;} // ignore spaces, tabs and newlines

    /* rest of the definitions given above can not be tokenized */
. { printf("SYNTAX ERROR: '%s' cannot be tokenized\n",yytext);} 

    /* definition of two enters to exit*/
[\n]{2}      { printf("Exit.\n"); return 0;}

%%
