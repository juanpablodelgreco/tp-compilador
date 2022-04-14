%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <conio.h>
    #include <string.h>
    FILE *yyin;
    int yylval;
%}

%option noyywrap
%option yylineno

DIGITO_CON_CERO     [0-9]
DIGITO_SIN_CERO     [1-9]
LETRA               [a-zA-Z]

PA			        "("
PC			        ")"
LLAVE_A             "{"
LLAVE_C             "}"
COMILLA             \"
POINT               "."

OP_AS   		    ":"
OP_PYC              ";"
OP_COMA             ","
OP_SUM		        "+"
OP_MUL              "*"
OP_DIV              "/"
OP_MENOR            "<"
OP_MAYOR            ">"
OP_MAYOR_IGUAL      ">="
OP_MENOR_IGUAL      "<="
OP_IGUAL            "=="
OP_DISTINTO         "!="

INTEGER             "Integer"
FLOAT_TYPE          "Float"
WRITE               "write"
WHILE               "while"
IF                  "if"
ELSE                "else"

A_BLOQUE            "DECVAR"
C_BLOQUE            "ENDDEC"

ID	                LETRA.(LETRA|DIGITO_CON_CERO)*
INT                 DIGITO.DIGITO_SIN_CERO.DIGITO_CON_CERO*
FLOAT               DIGITO_CON_CERO*.POINT.DIGITO_CON_CERO*.DIGITO_SIN_CERO
STRING              COMILLA.(LETRA|DIGITO_CON_CERO)*.COMILLA

%%
{PA}                {printf("%s\n",yytext);}
{PC}                {printf("%s\n",yytext);}
{LLAVE_A}           {printf("%s\n",yytext);}
{LLAVE_C}           {printf("%s\n",yytext);}
{COMILLA}           {printf("%s\n",yytext);}
{POINT}             {printf("%s\n",yytext);}
{OP_AS}             {printf("%s\n",yytext);}
{OP_PYC}            {printf("%s\n",yytext);}
{OP_COMA}           {printf("%s\n",yytext);}
{OP_SUM}            {printf("%s\n",yytext);}
{OP_MUL}            {printf("%s\n",yytext);}
{OP_DIV}            {printf("%s\n",yytext);}
{OP_MENOR}          {printf("%s\n",yytext);}
{OP_MAYOR}          {printf("%s\n",yytext);}
{OP_MENOR_IGUAL}    {printf("%s\n",yytext);}
{OP_MAYOR_IGUAL}    {printf("%s\n",yytext);}
{OP_IGUAL}          {printf("%s\n",yytext);}
{OP_DISTINTO}       {printf("%s\n",yytext);}
{INTEGER}           {printf("%s\n",yytext);}
{FLOAT_TYPE}        {printf("%s\n",yytext);}
{WRITE}             {printf("%s\n",yytext);}
{WHILE}             {printf("%s\n",yytext);}
{IF}                {printf("%s\n",yytext);}
{ELSE}              {printf("%s\n",yytext);}
{A_BLOQUE}          {printf("%s\n",yytext);}
{C_BLOQUE}          {printf("%s\n",yytext);}
{ID}                {printf("%s\n",yytext);}
{INT}               {printf("%s\n",yytext);}
{FLOAT}             {printf("%s\n",yytext);}
{STRING}            {printf("%s\n",yytext);}
"/n"
"/t"
"/r"
" "

%%

int main (int argc, char *argv[])
{
    if((yyin = fopen(argv[1],"rt")) == NULL)
    {
        printf("/nNo se pudo abrir el archivo: %s/n",argv[1]);
        return -1;
    }
    yylex();
    fclose(yyin);
    return 0;
}
        


