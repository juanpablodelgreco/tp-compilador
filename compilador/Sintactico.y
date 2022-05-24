%{
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include "y.tab.h"
int yystopparser = 0;
FILE  *yyin;

#define MAXTEXTSIZE 30
#define MAXTAMID 100

struct struct_tablaSimbolos{
	char nombre[100];
	char tipo[100];
	char valor[50];
	char longitud[100];
};

int guardar_TS(char*, char*);
int crear_TS();
void creacion_archivo_tabla();
void guardarTipoEnMatriz(char*);
void guardarNombreEnMatriz(char*);
char* buscarTipoConNombre(char*);
char* buscarTipoConPosiciones(int, int);
char * normalizarString(const char *);

char* matrizTipoVariable[100][100];
int punteroColumnaTipo = 0;
int punteroFilaTipo = 0;
char* matrizNombreVariable[100][100];
int punteroColumnaNombre = 0;
int punteroFilaNombre = 0;
char* matrizLongitudVariable[100][100];
int punteroColumnaLongitud = 0;
int punteroFilaLongitud = 0;
int cantvariables = 0;
int cantdetiposdevaribles = 0;

int puntero_array = 0;
struct struct_tablaSimbolos tablaSimbolos[1000];

char* tipoAsignacion;
int tipoLadoDerechoAsignacion;
char* variableLadoDerechoAsignacion;
char  sinComillas[100];	//La defino aca por que si no tira warning en normalizarString

%}

%token PROGRAM
%token VAR
%token DIM
%token AS
%token ENDVAR
%token BEGINP
%token ENDP
%token REAL
%token INTEGER
%token STRING
%token PUNTERO
%token DO
%token WHILE
%token ENDWHILE
%token IF
%token THEN
%token ELSE
%token ENDIF
%token FILTER
%token <str_val> CTE_STR
%token PUNTO
%token COMA
%token IGUAL
%token OP_COMP_IGUAL
%token OP_COMP_DIST
%token OP_COMP_MEN
%token OP_COMP_MAY
%token OP_COMP_MENIG
%token OP_COMP_MAYIG
%token OP_ASIG
%token CONCATENAR_STR
%token SIGNO_MAS
%token SIGNO_MENOS
%token SIGNO_POR
%token SIGNO_DIV
%token WRITE
%token READ
%token P_A
%token P_C
%token C_A
%token C_C
%token G_B
%token AND
%token OR
%token NOT
%token REF

%%

programa:
	PROGRAM {printf("Inicia COMPILADOR\n");} est_declaracion algoritmo{printf("Fin COMPILADOR ok\n"); creacion_archivo_tabla();}| 
	PROGRAM {printf("Inicia COMPILADOR\n");} algoritmo{printf("Fin COMPILADOR ok\n"); creacion_archivo_tabla(); }
	;

est_declaracion:
	   VAR {printf("\tDECLARACIONES\n");} declaraciones ENDVAR  {if(cantvariables!=cantdetiposdevaribles)
	   															yyerror("La cantidad de Variables definidas es distinta de la cantidad de tipos declarados.");
													    		printf("\tFin DECLARACIONES\n");};     

declaraciones :
			DIM lista_variables AS tipo_dato { printf("\t\tDECLARACION\n");}|
			DIM lista_variables AS tipo_dato declaraciones { printf("\t\tDECLARACION\n"); punteroFilaTipo++; punteroColumnaTipo = 0; punteroFilaNombre++; punteroColumnaNombre = 0; }

lista_variables: C_A listas C_C;

listas: id | id COMA listas;

id: ID {guardar_TS("ID",$1); guardarNombreEnMatriz($1);cantvariables++; }

listas2: id2 | id2 COMA listas2;

id2: ID;

tipo_dato: C_A tipos C_C;

tipos: tipo | tipo COMA tipos;

tipo: 	INTEGER { guardarTipoEnMatriz("INTEGER"); cantdetiposdevaribles++; }
		| STRING { guardarTipoEnMatriz("STRING"); cantdetiposdevaribles++;  }
		| REAL  { guardarTipoEnMatriz("REAL"); cantdetiposdevaribles++;  } 
		| PUNTERO {guardarTipoEnMatriz("PUNTERO"); cantdetiposdevaribles++; } ;

algoritmo: BEGINP {printf("\tBLOQUES\n");} bloque ENDP {printf("\tFIN BLOQUES\n");};


bloque: sentencia | sentencia bloque;


sentencia: ciclo | seleccion | asignacion | i_o | puntero;


i_o: READ{printf("\t\tREAD\n"); } ID | WRITE{printf("\t\tWRITE\n");} ID | WRITE{printf("\t\tWRITE\n");} CTE_STR;


ciclo: WHILE{printf("\t\tWHILE\n");} P_A condicion P_C DO bloque ENDWHILE{printf("\t\tFin WHILE\n");} ;


asignacion: 
			ID
			OP_ASIG
			expresion{ 

				tipoAsignacion = buscarTipoConNombre($1);

				if(strcmp(tipoAsignacion, "") == 0)
					yyerror(strcat($1, " no declarado."));

				if(tipoLadoDerechoAsignacion == 3){
					if(strcmp(buscarTipoConNombre(variableLadoDerechoAsignacion), "INTEGER") == 0)
						tipoLadoDerechoAsignacion = 0;
					if(strcmp(buscarTipoConNombre(variableLadoDerechoAsignacion), "REAL") == 0)
						tipoLadoDerechoAsignacion = 1;
					if(strcmp(buscarTipoConNombre(variableLadoDerechoAsignacion), "STRING") == 0)
						tipoLadoDerechoAsignacion = 2;
					if(strcmp(buscarTipoConNombre(variableLadoDerechoAsignacion), "PUNTERO") == 0)
						tipoLadoDerechoAsignacion = 3;
				}

				if(strcmp(tipoAsignacion, "INTEGER") == 0 && (tipoLadoDerechoAsignacion == 1 || tipoLadoDerechoAsignacion == 2 || tipoLadoDerechoAsignacion == 3))
					yyerror("Incompatibilidad de tipos.");
				if(strcmp(tipoAsignacion, "REAL") == 0 && (tipoLadoDerechoAsignacion == 2 || tipoLadoDerechoAsignacion == 3))
					yyerror("Incompatibilidad de tipos.");
				if(strcmp(tipoAsignacion, "STRING") == 0 && (tipoLadoDerechoAsignacion == 0 || tipoLadoDerechoAsignacion == 1 || tipoLadoDerechoAsignacion == 3))
					yyerror("Incompatibilidad de tipos.");
				if(strcmp(tipoAsignacion, "PUNTERO") == 0 && (tipoLadoDerechoAsignacion == 0 || tipoLadoDerechoAsignacion ==1 || tipoLadoDerechoAsignacion == 2 ))
					yyerror("Incompatibilidad de tipos.");

				printf("\t\tASIGNACION\n"); 
			};


seleccion: principio_if ENDIF{printf("\t\tFin IF\n");} | 
	principio_if ELSE bloque ENDIF {printf("\t\tFin IF con ELSE\n");};


principio_if: IF{printf("\t\tIF\n");} P_A condicion P_C THEN bloque;

filter: FILTER{printf("\t\tFILTER\n");} P_A condicion_especial COMA C_A listas2 C_C P_C;

condicion_especial: G_B op_comparacion expresion op_log condicion_especial 
			| G_B op_comparacion expresion;

condicion: comparacion 
         | comparacion op_log comparacion;


op_log: AND | OR; 


comparacion: comp | NOT comp;


comp: expresion op_comparacion expresion;


op_comparacion:	    OP_COMP_MEN
                 |  OP_COMP_MAY
                 |  OP_COMP_IGUAL
                 |  OP_COMP_MENIG
                 |  OP_COMP_MAYIG
                 |  OP_COMP_DIST;


expresion: termino | expresion op_sum_res termino;

op_sum_res: SIGNO_MAS | SIGNO_MENOS;


termino: factor | termino op_mult_div factor;


op_mult_div: SIGNO_POR | SIGNO_DIV; 

puntero: ID OP_ASIG	{ 
				tipoAsignacion = buscarTipoConNombre($1);

				if(strcmp(tipoAsignacion, "") == 0)
					yyerror(strcat($1, " no declarado."));

				if(strcmp(tipoAsignacion,"PUNTERO") != 0){
					yyerror("Se le quiere asignar a una variable que no es puntero un puntero");
				}
				printf("\t\tASIGNACION\n"); 
			} REF{printf("\t\tREF PUNTERO\n");} ID; 


factor: ID { guardar_TS("ID", $1);tipoLadoDerechoAsignacion = 3; variableLadoDerechoAsignacion = $1; }
	| CTE_INT { guardar_TS("CTEINTEGER", $1);tipoLadoDerechoAsignacion = 0; }
	| CTE_REAL { guardar_TS("CTEREAL", $1);tipoLadoDerechoAsignacion = 1; }
	| CTE_STR { 
			guardar_TS("CTESTRING", normalizarString($1)); 
			char* aux; 
			char aux2[50];
			//strcpy(aux2, "\\");
			//int length = strlen($1)-1;
			//printf("%d",length);
			//strncpy(aux, $1, length); 
			//printf("%s",aux);
			//aux[length]='\0';
			//strcat(aux2, aux);
			//strcat(aux2, "\\\"");
			//p00rintf("%s",aux2);
			tipoLadoDerechoAsignacion = 2;
		}
	| P_A expresion P_C{tipoLadoDerechoAsignacion = 0; }
	| filter

%%


int main(int argc,char *argv[]){
	if ((yyin = fopen(argv[1], "rt")) == NULL){
		printf("\nNo se puede abrir el archivo: %s\n", argv[1]);
	}else{
		yyparse();
	}
  	fclose(yyin);
  	return 0;
}
int yyerror(char* error){
	printf("Error: %s\n", error);
	exit(1);
}

int guardar_TS(char* tipo, char* nombre){
	int i;
	int retornar;
	for(i = 0; i < puntero_array; i++){
		if(strcmp(tablaSimbolos[i].nombre, nombre) == 0){
			return i;
		}
	}
	
	strcpy(tablaSimbolos[puntero_array].tipo, tipo);
	strcpy(tablaSimbolos[puntero_array].nombre, nombre);

	if (strcmp(tipo, "CTESTRING") == 0 )
	{
		int longi = strlen(nombre);
		char str[10];
		sprintf(str, "%d", longi);
		strcpy(tablaSimbolos[puntero_array].longitud,str);
	}
	retornar = puntero_array;
	puntero_array++;
	return retornar;
}

int crear_TS(){
	FILE *pf; 
	int i;
	pf = fopen("ts.txt","w"); 
	char* tipo;
	
	if (!pf)
		return 0;

	fprintf(pf, "Nombre\t\t\tTipo\t\t\tValor\t\t\tLongitud\n");

	for (i = 0; i < puntero_array; i++)
	{
		if (strcmp(tablaSimbolos[i].tipo, "ID") == 0 ){
			tipo = buscarTipoConNombre(tablaSimbolos[i].nombre);
			if(strcmp(tipo, "") != 0)
				fprintf(pf,"%s\t\t\t%s\n", tablaSimbolos[i].nombre, tipo);
		}
		else
		{	
			if(strcmp(tablaSimbolos[i].tipo, "CTESTRING") == 0 ){
				fprintf(pf,"_%s\t\t\t%s\t\t\t%s\t\t%s\n", tablaSimbolos[i].nombre, tablaSimbolos[i].tipo, tablaSimbolos[i].nombre,tablaSimbolos[i].longitud);			}
			else
			{
				fprintf(pf,"_%s\t\t\t%s\t\t\t%s\n", tablaSimbolos[i].nombre, tablaSimbolos[i].tipo, tablaSimbolos[i].nombre);
			}
		}
	}
	fclose(pf); 
	printf("TABLA DE SIMBOLOS creada\n");
	return 1;
}

void creacion_archivo_tabla(){
	if (crear_TS() == 0)
		printf("No se pudo crear el archivo de tabla de símbolos\n");
}

void guardarTipoEnMatriz(char* tipo){
	matrizTipoVariable[punteroFilaTipo][punteroColumnaTipo] = tipo;
	punteroColumnaTipo++;
}

void guardarNombreEnMatriz(char* nombre){


	matrizNombreVariable[punteroFilaNombre][punteroColumnaNombre] = nombre;
	punteroColumnaNombre++;

}

void guardarLongitudEnMatriz(char* nombre){


	matrizLongitudVariable[punteroFilaNombre][punteroColumnaNombre] = nombre;
	punteroColumnaNombre++;

}

char* buscarTipoConNombre(char* nombre){
	int i, j;
	char* tipo = "";
	for(i = 0; i <= punteroFilaNombre; i++){
		for(j = 0; j < 100; j++){
			if(matrizNombreVariable[i][j]&& strcmp(matrizNombreVariable[i][j], nombre) == 0 ){
				tipo = buscarTipoConPosiciones(i, j);
			}
		}
	}
	return tipo;
}

char* buscarTipoConPosiciones(int i, int j){
	char* tipo = matrizTipoVariable[i][j];
	if(tipo)
		return tipo;
	else
		return "";
}


char * normalizarString(const char *cteStringConComillas)	//le saca las comillas
{
	char *pos=strrchr(cteStringConComillas,'"');
	pos++;
	*pos='\0';
	
	char original[100];
	int i=0,j=0;
	strcpy(original,cteStringConComillas);
	
	while ( original[i]!='\0' )									
     {	
			if ( original[i]!='"' )							
			{
				sinComillas[j] = original[i];	
				j++;
            }
			i++;
     }
    sinComillas[j] = '\0';
	return sinComillas;
}
