%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "y.tab.h"

FILE  *yyin;
int yylval;
char *yyltext;
char memory[50];

int yyerror(void);
int checkInteger(char* str);
int checkFloat(char* str);
int checkString(char* str);

%}

%option noyywrap  
%option yylineno 

NUMBER		[0-9]
LETTER	    [a-zA-Z]
SPACE		[" "]
QUOTE   	"\""
ID			{LETTER}({LETTER}|{NUMBER})*
CTE_INT	    {NUMBER}+
CTE_FLOAT   ({NUMBER})+"."({NUMBER})*|({NUMBER})*"."({NUMBER})+
CTE_STR     ["].[^"]+["]
COMMENT     "-/"({LETTER}|{NUMBER}|_|SPACE|"*"|"/"|":"|"="|".")*"/-"
INTEGER     "INTEGER"|"integer"
Float       "FLOAT"|"float"
STRING      "STRING"|"string"
OP_AS   	":""="
OP_SUM		"+"
OP_MUL      "*"
OP_RES		"-"
OP_DIV      "/"
PA			"("
PC			")"

%%

{ID}			{printf("\nIdentificador: %s\n", yytext);return ID;}
{OP_SUM}		{printf("\nSuma: %s\n", yytext);return OP_SUM;}
{OP_AS}			{printf("\nAsignacion: %s\n", yytext);return OP_AS;}
{OP_MUL}		{printf("\nMultiplicacion: %s\n", yytext);return OP_MUL;}
{OP_RES}		{printf("\nResta: %s\n", yytext);return OP_RES;}
{OP_DIV}		{printf("\nDivision: %s\n", yytext);return OP_DIV;}
{PA}			{printf("\nParAbre: %s\n", yytext);return PA;}
{PC}			{printf("\nParCierra: %s\n", yytext);return PC;}
{COMMENT}       {   }
"\n"      		
"\t"
"\n\t"
" "             		
"\r\n"
.			    { printf( "ERROR LEXICO : Caracter no reconocido: %s\n", yytext ); exit (0);}

{CTE_INT}       {
                    if (checkInteger(yytext) == 1) {
                        printf("\nInteger const: %s\n", yytext);
                        yylval.num = yytext;
                            return CTE;
                    }
                }

{CTE_FLOAT}     {
                    if (checkFloat(yytext) == 1) {
                        printf("\nFloat const: %s\n", yytext);
                        yylval.doble = yytext;
							return CTE_FLOAT;
                    }
                }

{CTE_STR}     {
                    if (checkString(yytext) == 1) {
                        printf("\nString const: %s\n", yytext);
                        yylval.str_val = strdup(yytext);
							return CTE_STR;
                    }
                }

%%

int checkInteger(char* str)
{
    int aux = atoi(str);
      
    if(aux>=0 && aux<=65535) {
        return 1;            
    }

    fprintf(stderr, "Lexical error. Integer out of range -> \%s\n", str);
    fprintf(stderr, "Abort.\n");

    exit(0);
    return 0;
}

int checkFloat(char* str)
{
    int aux = atoi(str);
      
    if(sizeof(float) * CHAR_BIT == 32) {
        return 1;            
    }

    fprintf(stderr, "Lexical error. Float out of range -> \%s\n", str);
    fprintf(stderr, "Abort.\n");

    exit(0);
    return 0;
}

int checkString(char* str)
{
    if(strlen(str) <= 30) { // PREGUNTAR SI LAS COMILLAS CUENTAN COMO CARACTERES DE LA CADENA, DE SER ASI CAMBIAR 30 POR 32.
        return 1;            
    }

    fprintf(stderr, "Lexical error. String out of range -> \%s\n", str);
    fprintf(stderr, "Abort.\n");
    
    exit(0);
    return 0;
};