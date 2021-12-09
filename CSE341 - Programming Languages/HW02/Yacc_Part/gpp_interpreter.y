/**
 * @author Muhammed Oguz (muhammedogz)
 * @create date 08-12-2021 12:56
 * @modify date 08-12-2021 16:11
 * @desc Contains interpreter information
 */



%{
	#include <stdio.h>
	#include <string.h>
    #include <stdlib.h>

    #include "yacc_helper.h"
    int cond = 0;
%}

%union 
{
	int value;
	char *string;
	int values[1000];
}

%token <string> IDENTIFIER FILENAME
%token <value> VALUE
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_DEFVAR KW_FOR KW_IF KW_EXIT KW_LOAD
%token  KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV OP_DBLMULT OP_MULT OP_OP OP_CP OP_OC OP_CC COMMENT CUT
%token OP_COMMA
%start START




%type <value> INPUT
%type <values> LISTVALUE
%type <values> VALUES
%type <string> IDENTIFIERS
%type <value> EXPI
%type <value> EXPB
%type <values> EXPLISTI

%%

START: | START INPUT { printf("Cond: %d\n",cond); var1 = 0; var2 = 0; sum = 0;};


INPUT:
	EXPI
        {
            printf("--EXPI--\n");
            printSelector(&$$, cond, var2, isNil);
            if (isNil == 1) isNil = 0;
            $$ = $1;

        } |
	EXPLISTI 
        {
            printf("--EXPILISTI--\n");
            printSelector(&$$, cond, var2, 1);
            $<value>$ = $<value>1;
            if(cond == 3)
                copy($<values>$, $<values>1, var2);

		} |
	COMMENT {cond = 5;};

LISTVALUE:
	OP_OP KW_LIST VALUES OP_CP  { copy($<values>$, $<values>3, var1); cond = 3; } |
	CUT OP_OP VALUES OP_CP      { copy($<values>$, $<values>3, var1); cond = 3; };

VALUES:
	VALUES VALUE{ printf("-- values value --\n"); $$[var1] = $<value>2; var1 += 1; cond = 3; } | 
    VALUES IDENTIFIER{
        printf("-- values identifier --\n");
            int res = getIDentifierIndex($<string>2);
            if(res != -1)
            {
                $<value>2 = identifiers.values[res];
                cond = 0;
            }
            else
            {
                printf(" variable %s has no value\n", $<string>2);
                exit(0);
            }
            $<values>$[var1] = $<value>2;
            var1 += 1;
            cond = 3;
	    } |
	VALUE {printf("-- value --\n"); $$[var1] = $<value>1; var1 += 1; cond = 3;};

IDENTIFIERS:
	IDENTIFIERS IDENTIFIER{
	//toUpper($1);
	$$ = $1;
	strcat($$, " ");
	strcat($$, $2);
	cond = 1;
		  }
	| IDENTIFIER {
	//capitalSTR($<string>1);
	int index = getIDentifierIndex($1);
	if(index == -1)
	{
		strcpy(identifiers.identifier[identifiers.count], $<string>1);
		identifiers.values[identifiers.count] = -1;
		identifiers.count += 1;
	}
	$<string>$ = $<string>1; cond = 1;
		 };

EXPI:
	OP_OP OP_PLUS EXPI EXPI OP_CP           {$$ = $3 + $4; cond = 0;} |
	OP_OP OP_MINUS EXPI EXPI OP_CP          {$$ = $3 - $4; cond = 0;} |
	OP_OP OP_MULT EXPI EXPI OP_CP           {$$ = $3 * $4; cond = 0;} |
	OP_OP OP_DIV EXPI EXPI OP_CP            {checkZeroDivision($4); $$ = $3 / $4; cond = 0; } |
	OP_OP OP_DBLMULT EXPI EXPI OP_CP        {$$ = _pow($3, $4); cond = 0; } | 
    OP_OP IDENTIFIER EXPLISTI OP_CP         {$<string>$ = $<string>2; cond = 1;} |//capitalSTR($<string>2); 
	OP_OP IDENTIFIER EXPI OP_CP             {$<string>$ = $<string>2; cond = 1;} |//capitalSTR($<string>2); 
	OP_OP KW_SET IDENTIFIER EXPI OP_CP      { addIdentifier($3, $4); $$ = $4; cond = 0; } | 
	OP_OP KW_SET IDENTIFIER EXPLISTI OP_CP  { copy($<values>$, $<values>4, var2); cond = 3; } | 
    OP_OP KW_IF EXPB EXPLISTI OP_CP{
		if($3 == 1)
		{
            copy($<values>$, $<values>4, var2);
            cond = 3;
        }
		else
		{
            $$ = 0;
            cond = 3;
		}
	} |
    OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP{
        if($3 == 1)
        {
            copy($<values>$, $<values>4, var2);
            cond = 3;
        }
        else
        {
            copy($<values>$, $<values>5, var2);
            cond = 3;
        }
	} |
	OP_OP KW_IF EXPB EXPI OP_CP{
        if($3 == 1)
        {
            $$ = $4;
            cond = 0;
        }
        else
        {
            $$ = 0;
            cond = 3;
        }
    } |
	OP_OP KW_IF EXPB EXPI EXPI OP_CP{
		if($3 == 1)
		{
	        $$ = $4;
	        cond = 0;
		}
		else
		{
            $$ = $5;
            cond = 0;
		}
    } |
	OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP { copy($<values>$, $<values>8, var2); cond = 3; }|
	OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP{ addIdentifier($3, $4); $<string>$ = $3; cond = 1; } |
	OP_OP KW_DEFVAR IDENTIFIER EXPLISTI OP_CP{ $<string>$ = $<string>3; cond = 1; } |
	OP_OP KW_LOAD OP_OC FILENAME OP_CC OP_CP { $$ = loadFile($<string>4); cond = 2; } |
	OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPI OP_CP { addIdentifier($3, -1); $<string>$ = $3; cond =  1; } | 
    OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPLISTI OP_CP { addIdentifier($3, -1); $<string>$ = $3;  cond =  1; } |
	OP_OP KW_DEFFUN IDENTIFIER EXPLISTI OP_CP { addIdentifier($3, -1); $<string>$ = $3;  cond =  1; } |
	OP_OP KW_EXIT OP_CP {printf("Exiting.\n"); exit(0);} |
	OP_OP KW_DISP EXPI OP_CP { $$ = $3; } |
	OP_OP KW_DISP EXPLISTI OP_CP { copy($<values>$, $<values>3, var2); cond = 3; } |
	EXPB {$$ = $1;};

EXPB:
	OP_OP KW_AND EXPB EXPB OP_CP    {$$ = $3 && $4; cond = 2;} |
	OP_OP KW_OR EXPB EXPB OP_CP     {$$ = $3 || $4; cond = 2;} |
	OP_OP KW_NOT EXPB OP_CP         {if($3 == 0) $$ = 1; else $$ = 0; cond = 2;} |
    OP_OP KW_EQUAL EXPB EXPB OP_CP  {if($3 == $4) $$ = 1; else $$ = 0; cond = 2;} |
	OP_OP KW_LESS EXPB EXPB OP_CP   {if($3 < $4) $$ = 1; else $$ = 0; cond = 2;} |
	VALUE {$$ = $1; cond = 0;} |
    KW_TRUE {$$ = 1; cond = 2;} |
	KW_FALSE {$$ = 0; cond = 2;} |
	KW_NIL {$$ = 0; cond = 2; isNil = 1;} |
	IDENTIFIER {
        int index = getIDentifierIndex($1);
        if(index != -1)
        {
            $$ = identifiers.values[index];
            cond = 0;
        }
        else
        {
            printf("Variable %s not defined!\n", $1);
            exit(0);
        }
	};

EXPLISTI:
	OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {
	int j, x = 0;
	
    for(j = sum - var2; j < sum; j++, x++)
        {$<values>3[j] = $<values>4[x];}
        copy($<values>$, $<values>3, sum);
        cond = 3;
        var2 = sum;
        }
	| OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP {
	int j, x = 0;
	for(j = sum - var2; j < sum; j++, x++)
        {$<values>3[j] = $<values>4[x];}
        copy($<values>$, $<values>3, sum);
        cond = 3;
        var2 = sum;
        }
	| OP_OP KW_APPEND EXPI EXPLISTI OP_CP {
	int j;
	for(j = 0; j < var2; j++)
	    {$<values>$[j+1] = $<values>4[j];}

        $<values>$[0] = $<value>3;
        cond = 3;
        var2 += 1;
	} |
    LISTVALUE { copy($$, $1, var1); var2 = var1; sum += var1; cond = 3; var1 = 0; };

%%

int main(int argc, char *argv)
{

    int token = yyparse();
	return 0;
}



