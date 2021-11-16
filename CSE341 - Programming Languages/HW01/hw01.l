/* definition seciton */
%{

%}


/* rules seciton */
%%

[a-z]   {printf("a char\n");}
.       {printf("not a char\n");}
\n      {return 0;}

%%

yywrap() {}

int main()
{
    printf("Enter your string:");
    yylex();
    return 0;
}