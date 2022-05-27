%{
/* includes */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include "y.tab.h"
#include "y.tab.h"

/* defines */

#define MAX_REGS 1000
#define CADENA_MAXIMA 31
#define TRUE 1
#define FALSE 0
#define ERROR -1
#define OK 3

/* enums */
enum tipoSalto{
	normal,
	inverso
};

enum and_or{
	and,
	or,
	condicionSimple
};

enum tipoDato{
	tipoInt,
	tipoFloat,
	tipoString,
	sinTipo
};

typedef struct
{
	int cantExpresiones;
	int salto1;
	int salto2;
	int saltoElse;
	int nro;
	enum and_or andOr;
	enum tipoDato tipo;
}t_info;

enum tipoCondicion{
	condicionIf,
	condicionWhile
};

/* structs */

typedef struct {
    char lexeme[50];
    char datatype[50];
    char value[50];
    int length;
} t_symbol_table;

typedef struct
{
	char cadena[CADENA_MAXIMA];
	int nro;
}t_infoPolaca;

typedef struct s_nodoPolaca{
	t_infoPolaca info;
	struct s_nodoPolaca* psig;
}t_nodoPolaca;

typedef t_nodoPolaca *t_polaca;

typedef struct s_nodoPila{
    	t_info info;
    	struct s_nodoPila* psig;
	}t_nodoPila;

typedef t_nodoPila *t_pila;
t_pila pilaIf;
t_pila pilaWhile;

/* funciones */
void guardarPolaca(t_polaca*);
int ponerEnPolacaNro(t_polaca*,int, char *);
int ponerEnPolaca(t_polaca*, char *);
void crearPolaca(t_polaca*);
char* obtenerSalto(enum tipoSalto);

void vaciarPila(t_pila*);
t_info* sacarDePila(t_pila*);
void crearPila(t_pila*);
int ponerEnPila(t_pila*,t_info*);
t_info* topeDePila(t_pila*);

t_info* topeDePila(t_pila*);
t_info* sacarDePila(t_pila*);
int contadorIf=0;
int contadorWhile=0;
enum tipoCondicion tipoCondicion;

/* variables globales */

extern t_symbol_table symbol_table[MAX_REGS];

t_polaca polaca;
int contadorPolaca=0;
char ultimoComparador[3];
enum and_or ultimoOperadorLogico;

int indicesParaAsignarTipo[MAX_REGS];
int contadorListaVar=0;
int esAsignacion;
char tipoAsignacion[50];

int avgNumero=0;
int avg[MAX_REGS];
int contadorAvg[MAX_REGS];

extern char*yytext;
extern int yylineno;
int yystopparser=0;
FILE  *yyin;

  int yyerror();
  int yylex();

%}

%union {
	int vali;
	double valf;
	char*vals;
}

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
%token OP_NE
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
	declaraciones resto_programa {printf("resto_programa\n");}
	| declaraciones
	| resto_programa
	;

lista_variables:
	ID{int posicion=buscarEnTablaDeSimbolos($<vals>1); indicesParaAsignarTipo[contadorListaVar++]=posicion;}
	| lista_variables COMA ID{int posicion=buscarEnTablaDeSimbolos($<vals>3); indicesParaAsignarTipo[contadorListaVar++]=posicion;}
	;

bloque_declaraciones:
	lista_variables OP_DP tipo{contadorListaVar=0;}
	| bloque_declaraciones lista_variables OP_DP tipo{contadorListaVar=0;}
	;

tipo:
	INTEGER{
		while(contadorListaVar-- > 0)
		{
			if(strcmp("VARIABLE",symbol_table[indicesParaAsignarTipo[contadorListaVar]].datatype) == 0)
			{
				strcpy(symbol_table[indicesParaAsignarTipo[contadorListaVar]].datatype,"INTEGER");
			}
			else
			{
				yyerrormsg("ID repetida");
			}
		}
	}
	| FLOAT{
		while(contadorListaVar-- > 0)
		{
			if(strcmp("VARIABLE",symbol_table[indicesParaAsignarTipo[contadorListaVar]].datatype) == 0)
			{
				strcpy(symbol_table[indicesParaAsignarTipo[contadorListaVar]].datatype,"FLOAT");
			}
			else
			{
				yyerrormsg("ID repetida");
			}
		}
	}
	| STRING{
		while(contadorListaVar-- > 0)
		{
			if(strcmp("VARIABLE",symbol_table[indicesParaAsignarTipo[contadorListaVar]].datatype) == 0)
			{
				strcpy(symbol_table[indicesParaAsignarTipo[contadorListaVar]].datatype,"STRING");
			}
			else
			{
				yyerrormsg("ID repetida");
			}
		}
	}
	;

declaraciones:
	DECVAR bloque_declaraciones ENDDEC {printf("declaraciones\n");}
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
	ID{
		if(strcmp(symbol_table[buscarEnTablaDeSimbolos($<vals>1)].datatype,"VARIABLE")==0)
		{
			yyerrormsg("Variable sin declarar");
		}
		esAsignacion=1;
		strcpy(tipoAsignacion,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].datatype);
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);
		}OP_AS expresion{
			esAsignacion=0;
			strcpy(tipoAsignacion,"VARIABLE");
			ponerEnPolaca(&polaca,"=");
			} OP_ENDLINE {printf("ID = Expresion es ASIGNACION\n");}
	;

expresion:
	termino {printf("Termino es Expresion\n");}
	| expresion OP_SUM{
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
			{
				yyerrormsg("Operacion invalida en suma(Intenta asignar un numero a un string)");
			}
		}termino{ponerEnPolaca(&polaca,"+");} {printf("Expresion+Termino es Expresion\n");}
	| expresion OP_RES{
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
			{
				yyerrormsg("Operacion invalida en resta(Intenta asignar un numero a un string)");
			}
		} termino{ponerEnPolaca(&polaca,"-");} {printf("Expresion-Termino es Expresion\n");}
	| CTE_STR{
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")!=0)
			{
				yyerrormsg("Operacion invalida, Intenta asignar un string a un numero");
			}
		} {printf("CTE_STR es Expresion\n");}
	;

termino: 
   factor {printf("Factor es Termino\n");}
   | termino OP_MUL{
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
			{
				yyerrormsg("Operacion invalida en multiplicacion(multiplica un numero a un string)");
			}
		} factor{ponerEnPolaca(&polaca,"*");} {printf(" Termino*Factor es Termino\n");}
   | termino OP_DIV{
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
			{
				yyerrormsg("Operacion invalida en division(Divide un numero a un string)");
			}
		} factor{ponerEnPolaca(&polaca,"/");} {printf(" Termino/Factor es Termino\n");}
   ;
   
factor: 
	ID{
		int posicion=buscarEnTablaDeSimbolos($<vals>1);
		if(strcmp(symbol_table[posicion].datatype,"VARIABLE")==0)
		{
			yyerrormsg("Variable sin declarar");
		}
		if(esAsignacion==1&&strcmp(symbol_table[posicion].datatype,"STRING")==0&&strcmp(tipoAsignacion,"STRING")!=0)
		{
			yyerrormsg("Intenta asignar ID de distinto tipo(string)");
		}
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);
		printf("ID es Factor \n");}
	| CTE_INT{
		if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
		{
			yyerrormsg("Intenta asignar CTE de distinto tipo");
		}
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);
		printf("CTE es Factor\n");}
	| CTE_FLOAT{
		if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
		{
			yyerrormsg("Intenta asignar CTE de distinto tipo");
		}
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);
		printf("CTE es Factor\n");}
	| PA expresion PC {printf("Expresion entre parentesis es Factor\n");}
	| funcion {printf("funcion es Factor\n");}
	;
	
funcion:
	average {
		if(esAsignacion==1&&strcmp("FLOAT",tipoAsignacion)!=0)
		{
			yyerrormsg("Intenta asignar valor de distinto tipo");
		}
		printf(" average es funcion\n");}
	| between {
		if(esAsignacion==1)
		{
			yyerrormsg("Intenta asignar valor booleano");
		}
		printf(" between es funcion\n");}
	;

lista:
	CA elementos CC {printf(" [elementos] es una lista \n");}

elementos:
	elemento 
	| elemento COMA elementos 
	;
	
elemento:
	expresion
	{
		contadorAvg [avgNumero-1]++;
		if(contadorAvg[avgNumero-1] > 1){
			ponerEnPolaca(&polaca, "+");
		}
	}
	;
	
average:
	AVG 
	{
		contadorAvg[avgNumero] = 0;
		avgNumero++;
		
	}
	PA lista 
	PC {

		avgNumero--;
		char aux[20];
		sprintf(aux, "%d", contadorAvg[avgNumero]);
		ponerEnPolaca(&polaca, aux);
		ponerEnPolaca(&polaca, "/");
		printf(" AVG(lista) es Average\n");
	}
	;

between:
	BETWEEN PA expresion COMA CA expresion COMA expresion CC PC {printf(" BETWEEN(id,lista) es between\n");}
	;

write:
    WRITE CTE_STR OP_ENDLINE
	| WRITE ID OP_ENDLINE
    ;

read: 
	READ ID OP_ENDLINE
	;

operador_comparacion:
	OP_LOW
	{
		strcpy(ultimoComparador,$<vals>1);
	}
	| OP_GREAT
	{
		strcpy(ultimoComparador,$<vals>1);
	}
	| OP_EQUAL
	{
		strcpy(ultimoComparador,$<vals>1);
	}
	| OP_LE
	{
		strcpy(ultimoComparador,$<vals>1);
	}
	| OP_GE
	{
		strcpy(ultimoComparador,$<vals>1);
	}
	| OP_NE
	{
		strcpy(ultimoComparador,$<vals>1);
	}
	;

operador_logico:
	AND
	{
		ultimoOperadorLogico = and;
	}
	| OR
	{
		ultimoOperadorLogico = or;
	}
	;

operador_negacion:
	NOT
	;

condicion: comparacion
			{
				switch(tipoCondicion)
				{
					case condicionIf:
						ponerEnPolaca(&polaca,"CMP");
						ponerEnPolaca(&polaca,obtenerSalto(inverso));
						topeDePila(&pilaIf)->salto1=contadorPolaca;
						ponerEnPolaca(&polaca,"");
						topeDePila(&pilaIf)->andOr = condicionSimple;
						break;

					case condicionWhile:
						ponerEnPolaca(&polaca,"CMP");
						ponerEnPolaca(&polaca,obtenerSalto(inverso));
						topeDePila(&pilaWhile)->salto1=contadorPolaca;
						ponerEnPolaca(&polaca,"");
						topeDePila(&pilaWhile)->andOr = condicionSimple;
						break;
				}
			}
	|operador_negacion comparacion
			{
				switch(tipoCondicion)
				{
					case condicionIf:
						ponerEnPolaca(&polaca,"CMP");
						ponerEnPolaca(&polaca,obtenerSalto(normal));
						topeDePila(&pilaIf)->salto1=contadorPolaca;
						ponerEnPolaca(&polaca,"");
						topeDePila(&pilaIf)->andOr = condicionSimple;
						break;

					case condicionWhile:
						ponerEnPolaca(&polaca,"CMP");
						ponerEnPolaca(&polaca,obtenerSalto(normal));
						topeDePila(&pilaWhile)->salto1=contadorPolaca;
						ponerEnPolaca(&polaca,"");
						topeDePila(&pilaWhile)->andOr = condicionSimple;
						break;
				}
			}
	| comparacion operador_logico
			{
				switch(tipoCondicion)
				{
					case condicionIf:
						switch(ultimoOperadorLogico){
							case and:
								ponerEnPolaca(&polaca,"CMP");
								ponerEnPolaca(&polaca,obtenerSalto(inverso));
								topeDePila(&pilaIf)->salto1=contadorPolaca;
								ponerEnPolaca(&polaca,"");
								printf("%d", topeDePila(&pilaIf)->salto1);
								topeDePila(&pilaIf)->andOr = and;
								break;
							case or:
								ponerEnPolaca(&polaca,"CMP");
								ponerEnPolaca(&polaca,obtenerSalto(normal));
								topeDePila(&pilaIf)->salto1=contadorPolaca;
								ponerEnPolaca(&polaca,"");
								topeDePila(&pilaIf)->andOr = or;
								break;
						}
						break;

					case condicionWhile:
						switch(ultimoOperadorLogico){
							case and:
								ponerEnPolaca(&polaca,"CMP");
								ponerEnPolaca(&polaca,obtenerSalto(inverso));
								topeDePila(&pilaWhile)->salto1=contadorPolaca;
								ponerEnPolaca(&polaca,"");
								topeDePila(&pilaWhile)->andOr = and;
								break;

							case or:
								ponerEnPolaca(&polaca,"CMP");
								ponerEnPolaca(&polaca,obtenerSalto(normal));
								topeDePila(&pilaWhile)->salto1=contadorPolaca;
								ponerEnPolaca(&polaca,"");
								topeDePila(&pilaWhile)->andOr = or;
								break;
						}
						break;
				}
			}
			comparacion
				{
					switch(tipoCondicion)
					{
						case condicionIf:
							ponerEnPolaca(&polaca,"CMP");
							ponerEnPolaca(&polaca,obtenerSalto(inverso));
							topeDePila(&pilaIf)->salto2=contadorPolaca;
							ponerEnPolaca(&polaca,"");
							if(topeDePila(&pilaIf)->andOr == or){
								char aux[20];
								sprintf(aux, "%d", contadorPolaca);
								ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto1, aux);
							}
							break;

						case condicionWhile:
							ponerEnPolaca(&polaca,"CMP");
							ponerEnPolaca(&polaca,obtenerSalto(inverso));
							topeDePila(&pilaWhile)->salto2=contadorPolaca;
							ponerEnPolaca(&polaca,"");
							if(topeDePila(&pilaWhile)->andOr == or){
								char aux[20];
								sprintf(aux, "%d", contadorPolaca);
								ponerEnPolacaNro(&polaca, topeDePila(&pilaWhile)->salto1, aux);
							}
							break;
					}
	            }
comparacion:
	expresion operador_comparacion expresion
	| between
	;

while:
	WHILE{
			t_info info;
			info.nro=contadorWhile++;
			info.saltoElse=contadorPolaca;
			ponerEnPila(&pilaWhile,&info);
			tipoCondicion=condicionWhile;
			ponerEnPolaca(&polaca,"ET");
		} 
		PA condicion PC bloque_ejecucion	
		{
			char aux[20];
			sprintf(aux, "%d", topeDePila(&pilaWhile)->saltoElse);
			ponerEnPolaca(&polaca,"BI");
			ponerEnPolaca(&polaca, aux);
			sprintf(aux, "%d", contadorPolaca);
			switch (topeDePila(&pilaWhile)->andOr)
			{
			case condicionSimple:
				ponerEnPolacaNro(&polaca, topeDePila(&pilaWhile)->salto1, aux);
				break;
			case and:
				ponerEnPolacaNro(&polaca, topeDePila(&pilaWhile)->salto1, aux);
				ponerEnPolacaNro(&polaca, topeDePila(&pilaWhile)->salto2, aux);
			case or:
				ponerEnPolacaNro(&polaca, topeDePila(&pilaWhile)->salto2, aux);
				break;
			}
			sacarDePila(&pilaWhile);
		}
	;

if:
	IF 
		{
			t_info info;
			info.nro=contadorIf++;
			ponerEnPila(&pilaIf,&info);
			tipoCondicion=condicionIf;
		}
	PA condicion PC
	{

	}
	resto
	{
		sacarDePila(&pilaIf);
	}
	;

else:
	ELSE
	{
		char aux[20];
		sprintf(aux, "%d", contadorPolaca);
		switch (topeDePila(&pilaIf)->andOr)
		{
		case condicionSimple:
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto1, aux);
			break;
		case and:
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto1, aux);
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto2, aux);
		case or:
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto2, aux);
			break;
		}
	}
	bloque_ejecucion
	{
	
		char aux[20];
		sprintf(aux, "%d", contadorPolaca);
		ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->saltoElse, aux);
	}
	;

bloque_ejecucion:
	LLA resto_programa LLC
	;

resto: 
	bloque_ejecucion
	{
		char aux[20];
		sprintf(aux, "%d", contadorPolaca);
		
		switch (topeDePila(&pilaIf)->andOr)
		{
		case condicionSimple:
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto1, aux);
			break;
		case and:
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto1, aux);
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto2, aux);
		case or:
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto2, aux);
			break;
		}
	}
	| bloque_ejecucion
	{
		char aux[20];
		ponerEnPolaca(&polaca,"BI");
		topeDePila(&pilaIf)->saltoElse = contadorPolaca;
		ponerEnPolaca(&polaca, "");
		if(topeDePila(&pilaIf)->andOr != or){
			sprintf(aux, "%d", contadorPolaca);
			ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->salto1, aux);
		}
	}
	else
	;
%%

/* funciones */
int yyerror(void)
{
    printf("Error Sintactico\n");
	exit (1);
}

int yyerrormsg(const char * msg)
{
	printf("[Linea %d] ",yylineno);
	printf("Error Sintactico: %s\n",msg);
  	system ("Pause");
    exit (1);
}

/* primitivas de polaca */


void crearPolaca(t_polaca* pp)
{
    *pp=NULL;
}

int ponerEnPolaca(t_polaca* pp, char *cadena)
{
	printf("ponerEnPolaca: cadena %s\n",cadena);
    t_nodoPolaca* pn = (t_nodoPolaca*)malloc(sizeof(t_nodoPolaca));
    if(!pn)
    {
    	printf("ponerEnPolaca: Error al solicitar memoria\n");
        return ERROR;
    }
    t_nodoPolaca* aux;
    strcpy(pn->info.cadena,cadena);
    pn->info.nro=contadorPolaca++;
    pn->psig=NULL;
    if(!*pp)
    {
    	*pp=pn;
    	return OK;
    }
    else
    {
    	aux=*pp;
    	while(aux->psig)
        	aux=aux->psig;
        aux->psig=pn;
    	return OK;
    }
}

int ponerEnPolacaNro(t_polaca* pp,int pos, char *cadena)
{
	t_nodoPolaca* aux;
	aux=*pp;
    while(aux!=NULL && aux->info.nro<pos)
    {
    	aux=aux->psig;
    }
    if(aux->info.nro==pos)
    {
    	strcpy(aux->info.cadena,cadena);
    	return OK;
    }
    else
    {
    	printf("NO ENCONTRADO\n");
    	return ERROR;
    }
    return ERROR;
}

void guardarPolaca(t_polaca *pp)
{
	FILE*pt=fopen("intermedia.txt","w+");
	t_nodoPolaca* pn;
	if(!pt)
	{
		printf("Error al crear el archivo intermedio.\n");
		return;
	}
	while(*pp)
    {
        pn=*pp;
        fprintf(pt, "%s\n",pn->info.cadena);
        *pp=(*pp)->psig;
        free(pn);
    }
	
	fclose(pt);
}

char* obtenerSalto(enum tipoSalto tipo)
{
	switch(tipo)
	{
		case normal:
			if(strcmp(ultimoComparador,"==")==0)
				return("BEQ");
			if(strcmp(ultimoComparador,">")==0)
				return("BGT");
			if(strcmp(ultimoComparador,"<")==0)
				return("BLT");
			if(strcmp(ultimoComparador,">=")==0)
				return("BGE");
			if(strcmp(ultimoComparador,"<=")==0)
				return("BLE");
			if(strcmp(ultimoComparador,"!=")==0)
				return("BNE");
			break;

		case inverso:
			if(strcmp(ultimoComparador,"==")==0)
				return("BNE");
			if(strcmp(ultimoComparador,">")==0)
				return("BLE");
			if(strcmp(ultimoComparador,"<")==0)
				return("BGE");
			if(strcmp(ultimoComparador,">=")==0)
				return("BLT");
			if(strcmp(ultimoComparador,"<=")==0)
				return("BGT");
			if(strcmp(ultimoComparador,"!=")==0)
				return("BEQ");
			break;
	}
}
// Métodos pila
/* primitivas de pila */

void crearPila(t_pila* pp)
{
    *pp=NULL;
}

int ponerEnPila(t_pila* pp,t_info* info)
{
    t_nodoPila* pn=(t_nodoPila*)malloc(sizeof(t_nodoPila));
    if(!pn)
        return 0;
    pn->info=*info;
    pn->psig=*pp;
    *pp=pn;
    return 1;
}

t_info * sacarDePila(t_pila* pp)
{
	t_info* info = (t_info *) malloc(sizeof(t_info));
    if(!*pp){
    	return NULL;
    }
    *info=(*pp)->info;
    *pp=(*pp)->psig;
    return info;

}

void vaciarPila(t_pila* pp)
{
    t_nodoPila* pn;
    while(*pp)
    {
        pn=*pp;
        *pp=(*pp)->psig;
        free(pn);
    }
}

t_info* topeDePila(t_pila* pila)
{
	return &((*pila)->info);
}

int main(int argc, char *argv[])
{
	crearPila(&pilaIf);
	crearPolaca(&polaca);
    if ((yyin = fopen(argv[1], "rt")) == NULL) {
        printf("\nNo se puede abrir el archivo de prueba: %s\n", argv[1]);
    } else { 
    	yyparse();
    }
	
    // showSymbolTable(); Función para realizar el debug de la tabla de simbolos.
    crear_TS();
	fclose(yyin);
	guardarPolaca(&polaca);
  return 0;
}


