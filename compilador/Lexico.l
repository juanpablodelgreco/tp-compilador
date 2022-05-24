%{
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <math.h>
#include "y.tab.h"

char espacioMemoria[50];	//Para copiar yytext a yylval

int verificarString(char* );

%}

%option noyywrap  
%option yylineno 

NUM					[0-9]
LETRA				[a-zA-Z]
NUM_INT				({NUM})+
NUM_REAL			({NUM})+"."({NUM})*|({NUM})*"."({NUM})+
ID					{LETRA}({LETRA}|{NUM}|_{LETRA}|_{NUM})*
ESPACIO				[" "]
COMILLAS			"\""
CTE_STR				["].[^"]+["]
COMENT_SIMPLE		"-/"({LETRA}|{NUM}|_|{ESPACIO}|"*"|"/"|":"|"="|".")*"/-"
COMENT_DOBLE		"-/"({LETRA}|{NUM}|_|{ESPACIO}|"*"|"/"|":"|"="|"."|{COMENT_SIMPLE})*"/-"
AND					"AND"|"&&"|"and"
OR					"OR"|"or"|"\|\|"
NOT					"NOT"|"not"
PROGRAM				"PROGRAM"|"program"
VAR					"VAR"|"var"
DIM					"DIM"|"dim"
AS					"AS"|"as"
ENDVAR				"ENDVAR"|"endvar"
BEGINP				"BEGINP"|"beginp"
ENDP				"ENDP"|"endp"
REAL				"REAL"|"real"
INTEGER				"INTEGER"|"integer"
STRING				"STRING"|"string"
DO					"DO"|"do"
WHILE				"WHILE"|"while"
ENDWHILE			"ENDWHILE"|"endwhile"
IF					"IF"|"if"
THEN				"THEN"|"then"
ELSE				"ELSE"|"else"
ENDIF				"ENDIF"|"endif"
FILTER 				"FILTER"|"filter"
PUNTERO				"puntero"
REF					"ref"

			
%%
{PROGRAM}			{ return PROGRAM; }
{VAR}				{ return VAR; }
{DIM}				{ return DIM; }
{AS}				{ return AS; }
{ENDVAR}			{ return ENDVAR; }
{BEGINP}			{ return BEGINP; }
{ENDP}				{ return ENDP;  }
{REAL}				{ return REAL; }
{INTEGER}			{ return INTEGER; }
{STRING}			{ return STRING; }
{DO}				{ return DO; }
{WHILE}				{ return WHILE; }
{ENDWHILE}			{ return ENDWHILE; }
{IF}				{ return IF; }
{THEN}				{ return THEN; }
{ELSE}				{ return ELSE; }
{ENDIF}				{ return ENDIF; }
{FILTER} 			{ return FILTER; }
{PUNTERO} 			{ return PUNTERO;}
{REF} 				{ return REF;}

{COMENT_DOBLE}		{  }
{COMENT_SIMPLE}		{  }
"=="				{ return OP_COMP_IGUAL; }
"="					{ return IGUAL; }
"."					{ return PUNTO; }
","					{ return COMA; }
"!="				{ return OP_COMP_DIST; }
"<"					{ return OP_COMP_MEN; }
">"					{ return OP_COMP_MAY; }
"<="				{ return OP_COMP_MENIG; }
">="				{ return OP_COMP_MAYIG; }
":="				{ return OP_ASIG; }
"++"				{ return CONCATENAR_STR; }
"+"					{ return SIGNO_MAS; }
"-"					{ return SIGNO_MENOS; }
"*"					{ return SIGNO_POR; }
"/"					{ return SIGNO_DIV; }
"("					{ return P_A; }
")"					{ return P_C; }
"["					{ return C_A; }
"]"					{ return C_C; }
"_"					{ return G_B; }
"WRITE"				{ return WRITE; }
"READ"				{ return READ; }
{AND}				{ return AND; }
{OR}				{ return OR; }
{NOT}				{ return NOT; }
{ID}				{	
						if(verificarID(yytext)==0){
							yylval.str_val = strdup(yytext);
							return ID;
						}
					}

{NUM_INT}			{ (verificarEnteros(yytext)); return NUM_INT;}

{NUM_REAL}			{verificarReales(yytext); return CTE_REAL;}

{CTE_STR}			{	
						if(verificarString(yytext)==0)
						{
							yylval.str_val = strdup(yytext);
							return CTE_STR;
						}
					}				
			
			


"\n"
"\t"
" "

.                           { salErrorLex(); }

%%

int verificarID(char* cadena)
{      
    if(strlen(cadena) <= 15)
    {
        return 0;;            
    }
    else
    {
        fprintf(stderr, "Error lexico. ID fuera de rango (maximo 15 caracteres) -> \%s\n", cadena);
        
        fprintf(stderr, "Fin de ejecucion.\n");
        exit(1);
        return 1;
    }
}

int verificarReales(char* cadena)
{


    float aux = atof(cadena);
    char * ptrCad = cadena;
    int cantDecimales=0;


    if(aux==0.0)
        return 0;

    if(aux>=2.93874E-39  && aux<=3.4E+38)
    {

        //Ahora controlo los decimales
        while(*ptrCad != '.'){
            ptrCad++;  
        }
        ptrCad++;
        //Cuento cuandos decimales
        while(*ptrCad != '\0'){
            ptrCad++;
            cantDecimales++;
        }
        if(cantDecimales>5){
            printf("Error lexico. Cantidad de decimales fuera de rango (Maximo 5 decimales)\n");
            fprintf(stderr, "Fin de ejecucion.\n");
            exit(0);
            return 1;
        }
        else{
            return 0;
        }
        
    }
    else
    {
        fprintf(stderr, "Error lexico. Constante Real fuera de rango postivo -> \%s\n", cadena);
        
        fprintf(stderr, "Fin de ejecucion.\n");
        exit(0);
        return 1;
    }   
    
}

int verificarEnteros(char* cadena)
{
    int aux = atoi(cadena);
      
    if(aux<0 && aux>=65535)
    {
        fprintf(stderr, "Error lexico. Constante Int fuera de rango -> \%s\n", cadena);
        fprintf(stderr, "Fin de ejecucion.\n");
        return yyerror();          
    }
   
   return 0;
}

int verificarString(char* cadena)
{
    if(strlen(cadena) <= 32)
    {
        return 0;            
    }
    else
    {
        fprintf(stderr, "ERROR LEXICO. Constante String fuera de rango (maximo 30 caracteres) ->\%s\n", cadena);
        
        fprintf(stderr, "Fin de ejecucion.\n");
        exit(0);
        return 1;
    }
    
};

int salErrorLex(){

            {
                fprintf(stderr, "\nError lexico en la linea %d. Expresion ilegal.\n",yylineno);
                
                fprintf(stderr, "Fin de ejecucion.\n");
                system("PAUSE");
                exit(69);
        
            }
    }    


