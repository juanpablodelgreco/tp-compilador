%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
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
FLOAT       "FLOAT"|"float"
STRING      "STRING"|"string"
OP_AS   	":""="
OP_SUM		"+"
OP_MUL      "*"
OP_RES		"-"
OP_DIV      "/"
PA			"("
PC			")"
OP_LOW      "<"
OP_GREAT    ">"
OP_EQUAL    "=="
OP_LE       "<="
OP_GE       ">="    

WRITE   "WRITE"|"write"
READ    "READ"|"read"
IF      "IF"|"if"
ELSE    "ELSE"|"else"
AND     "AND"|"and"
OR      "OR"|"or"
NOT     "NOT"|"not"
DECVAR  "DECVAR"|"decvar"
ENDDEC  "ENDDEC"|"enddec"

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

{CTE_INT}       {checkInteger(yytext); return CTE_INT;}

{CTE_FLOAT}     {checkFloat(yytext); return CTE_FLOAT;} 

{CTE_STR}     {checkString(yytext); return CTE_STR;}
 
{WRITE}     { printf("\nToken %s\n",yytext); return WRITE;}
{READ}      { printf("\nToken %s\n",yytext); return READ;}
{IF}        { printf("\nToken %s\n",yytext); return IF;}
{ELSE}      { printf("\nToken %s\n",yytext); return ELSE;}
{AND}       { printf("\nToken %s\n",yytext); return AND;}
{OR}        { printf("\nToken %s\n",yytext); return OR;}
{NOT}       { printf("\nToken %s\n",yytext); return NOT;}
{DECVAR}    { printf("\nToken %s\n",yytext); return DECVAR;}
{ENDDEC}    { printf("\nToken %s\n",yytext); return ENDDEC;}

{OP_LOW}    { printf("\nMenorQue: %s\n",yytext); return OP_LOW;}
{OP_GREAT}  { printf("\nMayorQue: %s\n",yytext); return OP_GREAT;}
{OP_EQUAL}  { printf("\nIgualQue: %s\n",yytext); return OP_EQUAL;}
{OP_LE}     { printf("\nMenorIgualQue: %s\n",yytext); return OP_LE;}
{OP_GE}     { printf("\nMayorIgualQue: %s\n",yytext); return OP_GE;}

"\n"      		
"\t"
"\n\t"
" "             		
"\r\n"
.			    { printf( "ERROR LEXICO : Caracter no reconocido: %s\n", yytext ); exit (0);}

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
};

int checkFloat(char* str)
{
    int aux = atoi(str);
      
    if(sizeof(FLOAT) * CHAR_BIT == 32) {
        return 1;            
    }

    fprintf(stderr, "Lexical error. Float out of range -> \%s\n", str);
    fprintf(stderr, "Abort.\n");

    exit(0);
    return 0;
};

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