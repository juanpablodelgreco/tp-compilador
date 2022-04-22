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
%token READ
%token CTE_FLOAT
%token CTE_STR
%token AVG
%token BETWEEN
%token WRITE
%token IF
%token ELSE
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
%token FLOAT
%token CTE_INT
%token COMA

%%
sentencia:  	   
	asignacion {printf(" FIN\n");} ;

asignacion: 
	ID OP_AS expresion {printf("    ID = Expresion es ASIGNACION\n");}
	|ID OP_AS asignacion {printf("    ID = Asignacion es ASIGNACION\n");}
	|ID OP_AS funcion {printf("    ID = Funcion es ASIGNACION\n");}
	;

expresion:
	termino {printf("    Termino es Expresion\n");}
	|expresion OP_SUM termino {printf("    Expresion+Termino es Expresion\n");}
	|expresion OP_RES termino {printf("    Expresion-Termino es Expresion\n");}
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
	BETWEEN PA expresion COMA CA expresion COMA expresion CC PC

%%


int main(int argc, char *argv[])
{
    if((yyin = fopen(argv[1], "rt"))==NULL)
    {
        printf("\nNo se puede abrir el archivo de prueba: %s\n", argv[1]);
       
    }
    else
    { 
        
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

