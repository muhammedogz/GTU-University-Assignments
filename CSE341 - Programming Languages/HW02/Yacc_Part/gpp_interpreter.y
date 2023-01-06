%{
	#include <stdio.h>
	#include <string.h>
  #include <stdlib.h>
  #include <stdio.h>
  #include <string.h>
  #include <stdlib.h>

  extern FILE *yyin; // for printing to file

  // prevent simple yylex and yyerror warning. Not so impotant.
  int yylex(); 
  int yyerror(const char *error)
  {
      printf("Syntax Error\n");
      exit(1);
  }



  typedef enum {
      INT,
      STRING,
      BOOL,
      LIST
  } Type;

  Type type = INT;  // track type of input
  // keep identifier val
  struct IdentifStruct
  { 
      char identifier[1000][200]; 
      int values[1000]; 
      int count;
  };

  struct IdentifStruct identifiers = {"", 0, 0};

  int serachIdentifier(char *searchStr)
  {
      for(int i = 0; i < identifiers.count; i++)
          if (strcmp(searchStr, identifiers.identifier[i]) == 0)
              return i;

      return -1;
  }

  int searchIdentifierValue(char *searchStr)
  {
      int index = serachIdentifier(searchStr);
      if (index == -1)
      {
          printf("Identifier %s not defined!\n", searchStr);
          exit(0);
      }
      else
          return identifiers.values[index];
      
      return 0;
  }

  void pushIdentifier(char *identifier, int value)
  {
      int index = serachIdentifier(identifier);
      // if given identifier does not exist, add
      if(index == -1)
      {
          strcpy(identifiers.identifier[identifiers.count], identifier);
          identifiers.values[identifiers.count] = value;
          identifiers.count += 1;
      }
      // if already exist, update value
      else
          identifiers.values[index] = value;

  }

  // concatenate two int arrays and return first
  int* concatAppend(int *array1, int *array2, int sum, int size)
  {
      int i = 0;
    for(int j = sum - size; j < sum; j++, i++)
        array1[j] = array2[i];
      
      return array1;
  }

  // append the begining of the array
  void appendToBegggining(int *arr, int size, int value)
  {
      // shift all elements to the right
    for(int i = 0; i < size; i++)
        arr[i+1] = arr[i];

    arr[0] = value;
  }

  void copyArrayWithSize(int _arr1[], int _arr2[], int _size)
  {
      
      for(int i = 0; i < _size && i < 999; i++)
          _arr1[i] = _arr2[i];
  }


  // print content due to type
  void selectPrintTypeFunc(void* param, Type type, int size, int checkIsGivenValueIsList)
  {
      printf("Result: ");
      if (type == INT)
          printf("%d", *(int*)param);
      else if (type == STRING)
      {
          char **temp = (char*)param; char *str = (char*)temp[0];
          while (*str != '\0') { printf("%c", *str); str++; }
      }
      else if (type == BOOL)
      {
          if (checkIsGivenValueIsList == 0)
          {
              if (*(int*)param == 1)
                  printf("TRUE");
              else
                  printf("FALSE");
          }
          else 
          {
              if (*(int*)param == 1)
                  printf("T");
              else
                  printf("NIL");
          }
      }
      else if (type == LIST)
      {
          printf("(");
          for (int i = 0; i < size; i++)
          {
              printf("%d", *(int*)((char*)param + i * sizeof(int)));
              if (i != size - 1)
              {
                  printf(" ");
              }
          }
          printf(")");
      }
      printf("\n");
  }

  void checkValueIsZeroForDivision(int _value)
  {
      if (_value == 0)
      {
          printf("Zero Division Error.\n");
          exit(1);
      }
  }

  int _pow(int _base, int _exp)
  {
      int result = 1;
      for (int i = 0; i < _exp; i++)
          result *= _base;
      return result;
  }


  int var1 = 0;
  int var2 = 0;
  int sum = 0;
  int isNilValue = 0;
%}

%union 
{
	char *string;
	int value;
	int values[1000];
}


%token <string> IDENTIFIER
%token <value> VALUE
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_DEFVAR KW_FOR KW_IF KW_EXIT KW_LOAD
%token KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV OP_DBLMULT OP_MULT OP_OP OP_CP OP_OC OP_CC COMMENT CUT
%token OP_COMMA


%start START

%type <value> INPUT
%type <values> LISTVALUE
%type <values> VALUES
%type <string> FUNCPARAM
%type <value> EXPI
%type <value> EXPB
%type <values> EXPLISTI

%%

START: | START INPUT {var1 = 0; var2 = 0; sum = 0;};

INPUT:
	EXPI {      selectPrintTypeFunc(&$$, type, var2, isNilValue); if (isNilValue == 1) isNilValue = 0; $$ = $1; } |
	EXPLISTI {  selectPrintTypeFunc(&$$, type, var2, 1); $$ = $<value>1;
            if(type == 3) copyArrayWithSize($<values>$, $<values>1, var2); } |
	COMMENT { printf("Result: COMMENT\n"); };

LISTVALUE:
	OP_OP KW_LIST VALUES OP_CP  { copyArrayWithSize($<values>$, $<values>3, var1); type = LIST; } |
	CUT OP_OP VALUES OP_CP      { copyArrayWithSize($<values>$, $<values>3, var1); type = LIST; };

VALUES:
	VALUES VALUE{ $$[var1] = $<value>2; var1 += 1; type = LIST; } | 
	VALUE {$$[var1] = $<value>1; var1 += 1; type = LIST;};

FUNCPARAM:
	IDENTIFIER { pushIdentifier($1, 0); $$ = $1; type = STRING; };

EXPI:
	OP_OP OP_PLUS EXPI EXPI OP_CP           {$$ = $3 + $4; type = INT;} |
	OP_OP OP_MINUS EXPI EXPI OP_CP          {$$ = $3 - $4; type = INT;} |
	OP_OP OP_MULT EXPI EXPI OP_CP           {$$ = $3 * $4; type = INT;} |
	OP_OP OP_DIV EXPI EXPI OP_CP            {checkValueIsZeroForDivision($4); $$ = $3 / $4; type = INT; } |
	OP_OP OP_DBLMULT EXPI EXPI OP_CP        {$$ = _pow($3, $4); type = INT; } | 
    OP_OP IDENTIFIER EXPLISTI OP_CP         {$<string>$ = $<string>2; type = STRING;} |
	OP_OP IDENTIFIER EXPI OP_CP             {$$ = searchIdentifierValue($2) + $3; type = INT;} |
	OP_OP KW_SET IDENTIFIER EXPI OP_CP      { pushIdentifier($3, $4); $$ = $4; type = INT; } | 
	OP_OP KW_SET IDENTIFIER EXPLISTI OP_CP  { copyArrayWithSize($<values>$, $<values>4, var2); type = LIST; } | 
    OP_OP KW_IF EXPB EXPLISTI OP_CP{
		if($3 == 1) copyArrayWithSize($<values>$, $<values>4, var2);
		else        $$ = 0;
        type = LIST; } |
    OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP{
        if($3 == 1) copyArrayWithSize($<values>$, $<values>4, var2);
        else        copyArrayWithSize($<values>$, $<values>5, var2);
        type = LIST; } |
	OP_OP KW_IF EXPB EXPI OP_CP{
        if($3 == 1) { $$ = $4; type = INT; }
        else        { $$ = 0; type = LIST;  }      } |
	OP_OP KW_IF EXPB EXPI EXPI OP_CP{
		if($3 == 1) $$ = $4;
		else        $$ = $5;
        type = INT; } |
	OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP { copyArrayWithSize($<values>$, $<values>8, var2); type = LIST; }|
	OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP{ pushIdentifier($3, $4); $<string>$ = $3; type = STRING; } |
	OP_OP KW_DEFVAR IDENTIFIER EXPLISTI OP_CP{ $<string>$ = $<string>3; type = STRING; } |
	OP_OP KW_DEFFUN IDENTIFIER OP_OP FUNCPARAM OP_CP EXPI OP_CP { pushIdentifier($3, $7); $<string>$ = $3; type =  STRING; } | 
    OP_OP KW_DEFFUN IDENTIFIER OP_OP FUNCPARAM OP_CP EXPLISTI OP_CP { pushIdentifier($3, $<value>7); $<string>$ = $3;  type =  STRING; } |
	OP_OP KW_DEFFUN IDENTIFIER EXPLISTI OP_CP { pushIdentifier($3, $<value>4); $<string>$ = $3;  type =  STRING; } |
	OP_OP KW_DISP EXPI OP_CP { $$ = $3; } |
	OP_OP KW_DISP EXPLISTI OP_CP { copyArrayWithSize($<values>$, $<values>3, var2); type = LIST; } |
	OP_OP KW_EXIT OP_CP {printf("Exiting.\n"); exit(0);} |
	EXPB {$$ = $1;};

EXPB:
	OP_OP KW_AND EXPB EXPB OP_CP    {$$ = $3 && $4; type = BOOL;} |
	OP_OP KW_OR EXPB EXPB OP_CP     {$$ = $3 || $4; type = BOOL;} |
	OP_OP KW_NOT EXPB OP_CP         {if($3 == 0) $$ = 1; else $$ = 0; type = BOOL;} |
    OP_OP KW_EQUAL EXPB EXPB OP_CP  {if($3 == $4) $$ = 1; else $$ = 0; type = BOOL;} |
	OP_OP KW_LESS EXPB EXPB OP_CP   {if($3 < $4) $$ = 1; else $$ = 0; type = BOOL;} |
	VALUE {$$ = $1; type = INT;} |
    KW_TRUE {$$ = 1; type = BOOL;} |
	KW_FALSE {$$ = 0; type = BOOL;} |
	KW_NIL {$$ = 0; type = BOOL; isNilValue = 1;} |
	IDENTIFIER { $$ = searchIdentifierValue($<string>1); type = INT; };

EXPLISTI:
	OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP   { copyArrayWithSize($$, concatAppend($3, $4, sum, var2), sum); type = LIST; var2 = sum; } |
	OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP   { copyArrayWithSize($$, concatAppend($3, $4, sum, var2), sum); var2 = sum; type = LIST;} |
	OP_OP KW_APPEND EXPI EXPLISTI OP_CP       { appendToBegggining($4, var2, $3); var2 += 1; type = LIST; } |
	LISTVALUE { copyArrayWithSize($<values>$, $<values>1, var1); var2 = var1; sum += var1; var1 = 0;  type = LIST; };

%%
int main(int argc, char *argv[])
{
    if (argc == 1)
    {
        printf("--- Start Typing --- (Type (exit) to exit) \n");
    }
    else if (argc == 2)
    {
        yyin = fopen(argv[1], "r");
        if (yyin == NULL)
        {
            printf("File not found\n");
            return 0;
        }
    }
    else 
    {
        printf("invalid args\n");
        return 0;
    }

    yyparse();
}