%{
#include <stdio.h>
#include <stdlib.h>

#include "sintactico.tab.h"
FILE  *yyin;
int yylval;
char *yyltext;


int yyerror(void);

%}

%option noyywrap  
%option yylineno 

DIGITO			[0-9]
LETRA			[a-zA-Z]

CTE		        {DIGITO}+
ID			{LETRA}({LETRA}|{DIGITO})*
OP_AS   		":""="
OP_SUM		        "+"
OP_MUL                  "*"
OP_RES		        "-"
OP_DIV                  "/"
PA			"("
PC			")"

%%

{CTE}			{printf("\nConstante: %s\n", yytext);return CTE;}
{ID}			{printf("\nIdentificador: %s\n", yytext);return ID;}
{OP_SUM}		{printf("\nSuma: %s\n", yytext);return OP_SUM;}
{OP_AS}			{printf("\nAsignacion: %s\n", yytext);return OP_AS;}
{OP_MUL}		{printf("\nMultiplicacion: %s\n", yytext);return OP_MUL;}
{OP_RES}		{printf("\nResta: %s\n", yytext);return OP_RES;}
{OP_DIV}		{printf("\nDivision: %s\n", yytext);return OP_DIV;}
{PA}			{printf("\nParAbre: %s\n", yytext);return PA;}
{PC}			{printf("\nParCierra: %s\n", yytext);return PC;}
"\n"      		
"\t"
"\n\t"
" "             		
"\r\n"
.			 { printf( "ERROR LEXICO : Caracter no reconocido: %s\n", yytext ); exit (0);}

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
        
