// Usa Lexico_ClasePractica
//Solo expresiones sin ()
%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"
int yystopparser=0;
FILE  *yyin;

  int yyerror();
  int yylex();

%}

%token ID
%token OP_AS
%token OP_SUM
%token OP_MUL
%token OP_RES
%token OP_DIV
%token PA
%token PC
%token CA
%token CC
%token LLA
%token LLC
%token READ
%token CTE_FLOAT
%token CTE_STR
%token AVG
%token BETWEEN
%token WRITE
%token IF
%token ELSE
%token WHILE
%token AND
%token OR
%token NOT
%token DECVAR
%token ENDDEC
%token OP_LOW
%token OP_GREAT
%token OP_EQUAL
%token OP_LE
%token OP_GE
%token CHAR_BIT
%token CTE_INT
%token COMA
%token OP_ENDLINE
%token INTEGER
%token FLOAT
%token STRING
%token QUOTE
%token OP_DP

%%
programa: 
	main {printf("Compilacion exitosa\n");};

main: 
	DECVAR bloque_declaraciones ENDDEC resto_programa {printf("resto_programa\n");};

lista_variables:
	ID | lista_variables COMA ID 
	;

bloque_declaraciones:
	lista_variables OP_DP tipo
	| bloque_declaraciones lista_variables OP_DP tipo
	;

tipo:
	INTEGER | FLOAT | STRING
	;

resto_programa: 
	sentencia {printf("sentencia\n");} 
	| resto_programa sentencia {printf("resto_programa sentencia\n");};

sentencia:  	   
	asignacion {printf(" asignacion\n");} 
	| if {printf(" if\n");}
	| while {printf(" while\n");}
	| write {printf(" write\n");}
	| read {printf(" read\n");}
	;

asignacion: 
	ID OP_AS expresion OP_ENDLINE {printf("    ID = Expresion es ASIGNACION\n");}
	|ID OP_AS asignacion OP_ENDLINE {printf("    ID = Asignacion es ASIGNACION\n");}
	|ID OP_AS funcion OP_ENDLINE {printf("    ID = Funcion es ASIGNACION\n");}
	;

expresion:
	termino {printf("    Termino es Expresion\n");}
	|expresion OP_SUM termino {printf("    Expresion+Termino es Expresion\n");}
	|expresion OP_RES termino {printf("    Expresion-Termino es Expresion\n");}
	|CTE_STR {printf("    CTE_STR es Expresion\n");}
	;

termino: 
   factor {printf("    Factor es Termino\n");}
   |termino OP_MUL factor {printf("     Termino*Factor es Termino\n");}
   |termino OP_DIV factor {printf("     Termino/Factor es Termino\n");}
   ;
   
factor: 
	ID {printf("    ID es Factor \n");}
	|CTE_INT {printf("    CTE es Factor\n");}
	|CTE_FLOAT {printf("    CTE es Factor\n");}
	|PA expresion PC {printf("    Expresion entre parentesis es Factor\n");}
	|funcion {printf("    funcion es Factor\n");}
	;
	
funcion:
	average {printf("     average es funcion\n");}
	|between {printf("     between es funcion\n");}
	;

lista:
	CA elementos CC {printf("     [elementos] es una lista \n");}

elementos:
	elemento 
	|elemento COMA elementos 
	;
	
elemento:
	expresion
	;
	
average:
	AVG PA lista PC {printf("     AVG(lista) es Average\n");}
	;

between:
	BETWEEN PA expresion COMA CA expresion COMA expresion CC PC {printf("     BETWEEN(id,lista) es between\n");}
	;

write:
    WRITE CTE_STR
	|WRITE ID
    ;

read: 
	READ ID
	;

operador_comparacion:
	OP_LOW
	| OP_GREAT
	| OP_EQUAL
	| OP_LE
	| OP_GE
	;

operador_logico:
	AND
	| OR
	;

operador_negacion:
	NOT
	;

comparacion:
	expresion operador_comparacion expresion
	| expresion operador_comparacion expresion operador_comparacion expresion
	| between
	;

while:
	WHILE PA operador_negacion PA comparacion PC PC bloque_ejecucion
	| WHILE PA comparacion PC bloque_ejecucion
	| WHILE PA comparacion operador_logico comparacion PC bloque_ejecucion
	;

if:
	IF PA operador_negacion PA comparacion PC PC bloque_ejecucion
	| IF PA operador_negacion PA comparacion PC PC bloque_ejecucion else
	| IF PA comparacion PC bloque_ejecucion
	| IF PA comparacion PC bloque_ejecucion else
	| IF PA comparacion operador_logico comparacion PC bloque_ejecucion
	| IF PA comparacion operador_logico comparacion PC bloque_ejecucion else
	;

else:
	ELSE bloque_ejecucion
	;

bloque_ejecucion:
	LLA resto_programa LLC
	;
%%

int main(int argc, char *argv[])
{
    if ((yyin = fopen(argv[1], "rt")) == NULL) {
        printf("\nNo se puede abrir el archivo de prueba: %s\n", argv[1]);
	
    } else { 
    	yyparse();
    }

	fclose(yyin);
  return 0;
}

int yyerror(void)
{
    printf("Error Sintactico\n");
	exit (1);
}

