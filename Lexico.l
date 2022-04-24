%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "y.tab.h"
#define MAX_REGS 1000

typedef struct {
    char lexeme[50];
    char datatype[50];
    char value[50];
    int length;
} t_symbol_table;

t_symbol_table symbol_table[MAX_REGS];
int table_regs = 0;
FILE  *yyin;
int yylval;
char *yyltext;
char memory[50];
	
int yyerror(void); // ?? esto
int checkInteger(char* str);
int checkFloat(char* str);
int checkString(char* str);
void pushSymbolTable(char* str, int is_cte);
void showSymbolTable();

%}

%option noyywrap  
%option yylineno 

NUMBER		[0-9]
LETTER	    [a-zA-Z]
ID			{LETTER}({LETTER}|{NUMBER})*
CTE_INT	    {NUMBER}+
CTE_FLOAT   ({NUMBER})+"."({NUMBER})*|({NUMBER})*"."({NUMBER})+
CTE_STR     {QUOTE}.[^"]+.{QUOTE}
COMMENT     "-/"({LETTER}|{NUMBER}|_|SPACE|"*"|"/"|":"|"="|".")*"/-"
INTEGER     "INTEGER"|"integer"
FLOAT       "FLOAT"|"float"
STRING      "STRING"|"string"
COMA		","
OP_AS   	":="|"="
OP_SUM		"+"
OP_MUL      "*"
OP_RES		"-"
OP_DIV      "/"
OP_DP       ":"
PA			"("
PC			")"
CA			"["
CC			"]"
LLA         "{"
LLC         "}"
OP_LOW      "<"
OP_GREAT    ">"
OP_EQUAL    "=="
OP_LE       "<="
OP_GE       ">="
OP_ENDLINE  ";"
QUOTE   	"\""

WRITE   "WRITE"|"write"
READ    "READ"|"read"
IF      "IF"|"if"
WHILE   "WHILE"|"while"
ELSE    "ELSE"|"else"
AND     "AND"|"and"
OR      "OR"|"or"
NOT     "NOT"|"not"
DECVAR  "DECVAR"|"decvar"
ENDDEC  "ENDDEC"|"enddec"
AVG 	"AVG"|"avg"
BETWEEN 	"BETWEEN"|"between"


%%

{OP_SUM}		{printf("\nSuma: %s\n", yytext);return OP_SUM;}
{OP_AS}			{printf("\nAsignacion: %s\n", yytext);return OP_AS;}
{OP_MUL}		{printf("\nMultiplicacion: %s\n", yytext);return OP_MUL;}
{OP_RES}		{printf("\nResta: %s\n", yytext);return OP_RES;}
{OP_DIV}		{printf("\nDivision: %s\n", yytext);return OP_DIV;}
{PA}			{printf("\nParAbre: %s\n", yytext);return PA;}
{PC}			{printf("\nParCierra: %s\n", yytext);return PC;}
{CA}			{printf("\nCorAbre: %s\n", yytext);return CA;}
{CC}			{printf("\nCorCierra: %s\n", yytext);return CC;}
{LLA}			{printf("\nLlaveAbre: %s\n", yytext);return LLA;}
{LLC}			{printf("\nLlaveCierra: %s\n", yytext);return LLC;}
{COMMENT}       {   }
{OP_ENDLINE}	{printf("\nEndLine: %s\n", yytext);return OP_ENDLINE;}
{INTEGER}		{printf("\nInteger: %s\n", yytext);return INTEGER;}
{FLOAT}		    {printf("\nFloat: %s\n", yytext);return FLOAT;}
{STRING}		{printf("\nString: %s\n", yytext);return STRING;}

{CTE_INT}       {checkInteger(yytext); printf("\nConstante: %s\n", yytext); pushSymbolTable(yytext,1); return CTE_INT;}

{CTE_FLOAT}     {checkFloat(yytext); printf("\nConstante: %s\n", yytext); pushSymbolTable(yytext,1); return CTE_FLOAT;} 

{CTE_STR}     {checkString(yytext); printf("\nConstante: %s\n", yytext);  pushSymbolTable(yytext,1); return CTE_STR;}

 
{WRITE}     { printf("\nToken %s\n",yytext); return WRITE;}
{READ}      { printf("\nToken %s\n",yytext); return READ;}
{IF}        { printf("\nToken %s\n",yytext); return IF;}
{ELSE}      { printf("\nToken %s\n",yytext); return ELSE;}
{WHILE}     { printf("\nToken %s\n",yytext); return WHILE;}
{AND}       { printf("\nToken %s\n",yytext); return AND;}
{OR}        { printf("\nToken %s\n",yytext); return OR;}
{NOT}       { printf("\nToken %s\n",yytext); return NOT;}
{DECVAR}    { printf("\nToken %s\n",yytext); return DECVAR;}
{ENDDEC}    { printf("\nToken %s\n",yytext); return ENDDEC;}
{COMA}    	{ printf("\nToken %s\n",yytext); return COMA;}
{AVG}		{ printf("\nToken %s\n",yytext); return AVG;}
{BETWEEN}	{ printf("\nToken %s\n",yytext); return BETWEEN;}

{OP_LOW}    { printf("\nMenorQue: %s\n",yytext); return OP_LOW;}
{OP_GREAT}  { printf("\nMayorQue: %s\n",yytext); return OP_GREAT;}
{OP_EQUAL}  { printf("\nIgualQue: %s\n",yytext); return OP_EQUAL;}
{OP_LE}     { printf("\nMenorIgualQue: %s\n",yytext); return OP_LE;}
{OP_GE}     { printf("\nMayorIgualQue: %s\n",yytext); return OP_GE;}
{QUOTE}     { printf("\nComillas: %s\n",yytext); return QUOTE;}
{OP_DP}     { printf("\nDos puntos: %s\n",yytext); return OP_DP;}

{ID}			{printf("\nIdentificador: %s\n", yytext); pushSymbolTable(yytext,0); return ID;}

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

void pushSymbolTable(char* str, int is_cte){
    int i=0, duplicate=0;
    while(i<table_regs && i<MAX_REGS && !duplicate)
    {
        if(!strcmp(symbol_table[i].lexeme,str)){
            duplicate = 1;
        }
        i++;
    }
    if(!duplicate)
    {
        symbol_table[table_regs].length = strlen(str);
        strcpy(symbol_table[table_regs].lexeme,str);
        if(is_cte){
            strcpy(symbol_table[table_regs].value,str);
        }
        table_regs++;
    }
};

int main(int argc, char *argv[])
{
    if ((yyin = fopen(argv[1], "rt")) == NULL) {
        printf("\nNo se puede abrir el archivo de prueba: %s\n", argv[1]);
	
    } else { 
    	yyparse();
    }
    // symbolTableToFile function
    showSymbolTable();
	fclose(yyin);
  return 0;
}

void showSymbolTable(){
    int i;
    printf("\n---Symbol Table---\n");
    printf("\n%8s\t%8s\t%8s\t%8s\n","lexeme","value","datatype","length");
    for(i=0; i<table_regs; i++){
        printf("\n%8s\t%8s\t%8s\t%8d\n",symbol_table[i].lexeme,symbol_table[i].value,symbol_table[i].datatype,symbol_table[i].length);
    }
}