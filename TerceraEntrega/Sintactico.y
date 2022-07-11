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
#define CADENA_MAXIMA 100
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
	char *cadena;
	int cantExpresiones;
	int salto1;
	int salto2;
	int saltoElse;
	int nro;
	enum and_or andOr;
	enum tipoDato tipo;
	char *dataType;
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

typedef t_nodoPolaca* t_polaca;

typedef struct s_nodoPila{
    	t_info info;
    	struct s_nodoPila* psig;
	}t_nodoPila;

typedef t_nodoPila* t_pila;
t_pila pilaIf;
t_pila pilaWhile;
t_pila pilaASM;

/* funciones */
void guardarPolaca(t_polaca*);
int ponerEnPolacaNro(t_polaca*,int, char *);
int ponerEnPolaca(t_polaca*, char *);
void crearPolaca(t_polaca*);
char* obtenerSalto(enum tipoSalto);
void generarAssembler(t_polaca*);

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

FILE * pf;

/* variables globales */
extern t_symbol_table symbol_table[MAX_REGS];
t_polaca polaca;
t_polaca polacaASM;
int contadorPolaca=0;
char ultimoComparador[3];
enum and_or ultimoOperadorLogico;
extern table_regs;

int indicesParaAsignarTipo[MAX_REGS];
int contadorListaVar=0;
int esAsignacion;
char tipoAsignacion[50];
char tipoDeDatoComparacion[7]="none";
int avgNumero=0;
int esAvg=0;
int avg[MAX_REGS];
int contadorAvg[MAX_REGS];
char auxBetween [100];
int isIf = 0;
extern char*yytext;
extern int yylineno;
int yystopparser=0;
int huboAsignacion=TRUE;
char ultimoTipo[30]="none";
int auxiliaresNecesarios=0;
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
	termino  {
		if(
		strcmp(tipoDeDatoComparacion, "none") != 0
		&& esAvg == 0
		&& strcmp(tipoDeDatoComparacion, symbol_table[buscarEnTablaDeSimbolos($<vals>1)].datatype) != 0){
				yyerrormsg("Error al comparar variables de distinto tipo.");
		}
		strcpy(tipoDeDatoComparacion, "none");
		printf("Termino es Expresion\n");
	}
	| expresion OP_SUM{
			auxiliaresNecesarios++;
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
			{
				yyerrormsg("Operacion invalida en suma(Intenta asignar un numero a un string)");
			}
		}termino{ponerEnPolaca(&polaca,"+");} {printf("Expresion+Termino es Expresion\n");}
	| expresion OP_RES{
			auxiliaresNecesarios++;
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
			{
				yyerrormsg("Operacion invalida en resta(Intenta asignar un numero a un string)");
			}
		} termino{ponerEnPolaca(&polaca,"-");} {printf("Expresion-Termino es Expresion\n");}
	
	;

termino: 
   factor {printf("Factor es Termino\n");}
   | termino OP_MUL{
			auxiliaresNecesarios++;
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
			{
				yyerrormsg("Operacion invalida en multiplicacion(multiplica un numero a un string)");
			}
		} factor{ponerEnPolaca(&polaca,"*");} {printf(" Termino*Factor es Termino\n");}
   | termino OP_DIV{
			auxiliaresNecesarios++;
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
        if(esAsignacion==1&&strcmp(tipoAsignacion,"INTEGER")==0&&strcmp(symbol_table[posicion].datatype,"FLOAT")==0)
        {
            yyerrormsg("Intenta asignar variable float a un int");
        }
        ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);
        printf("    ID es Factor \n");}
    | CTE_INT{
        if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
        {
            yyerrormsg("Intenta asignar CTE de distinto tipo");
        }
        ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);
        printf("    CTE es Factor\n");}
    | CTE_FLOAT{
        if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")==0)
        {
            yyerrormsg("Intenta asignar CTE de distinto tipo");
        }
        if(esAsignacion==1&&strcmp(tipoAsignacion,"INTEGER")==0)
        {
            yyerrormsg("Intenta asignar CTE float a un int");
        }
        ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);
        printf("    CTE es Factor\n");}
		| CTE_STR{
			if(esAsignacion==1&&strcmp(tipoAsignacion,"STRING")!=0)
			{
				yyerrormsg("Operacion invalida, Intenta asignar un string a un numero");
			}
		} {ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].lexeme);printf("CTE_STR es Expresion\n");}
    | PA expresion PC {printf("    Expresion entre parentesis es Factor\n");}
    | funcion {printf("    funcion es Factor\n");}
    ;
	
funcion:
	average {
		if(esAsignacion==1&&strcmp("FLOAT",tipoAsignacion)!=0)
		{
			yyerrormsg("Intenta asignar valor de distinto tipo");
		}
		esAvg=1;
		printf(" average es funcion\n");}

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
			auxiliaresNecesarios++;
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
		auxiliaresNecesarios++;
		printf(" AVG(lista) es Average\n");
		esAvg=0;
	}
	;

between:
	BETWEEN PA ID 
	{	
		if(strcmp(symbol_table[buscarEnTablaDeSimbolos($<vals>3)].datatype,"INTEGER")!=0){
			yyerrormsg("La variable de entrada debe ser de tipo INTEGER.");
		}
		strcpy(auxBetween,symbol_table[buscarEnTablaDeSimbolos($<vals>3)].lexeme);
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>3)].lexeme);
	}
	COMA rango PC {printf(" BETWEEN(id,lista) es between\n");}
	;

	rango:	CA expresion OP_ENDLINE {
		
			ponerEnPolaca(&polaca,"CMP");
			ponerEnPolaca(&polaca,"BLT");
			t_info info;
			info.salto1 = contadorPolaca;
			if(isIf){
				ponerEnPila(&pilaIf, &info);
			}else{
				ponerEnPila(&pilaWhile, &info);
			}
			ponerEnPolaca(&polaca,"");
			ponerEnPolaca(&polaca,auxBetween);
	} 
	expresion CC
	{
		ponerEnPolaca(&polaca,"CMP");
		ponerEnPolaca(&polaca,"BGT");
		t_info info;
		info.salto2 = contadorPolaca;
		info.andOr = and;
		if(isIf){
			ponerEnPila(&pilaIf, &info);
			isIf=0;
		}else{
			ponerEnPila(&pilaWhile, &info);
		}
		ponerEnPolaca(&polaca,"");
	}
	;

write:
    WRITE CTE_STR {
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>2)].lexeme);
		ponerEnPolaca(&polaca,"WRITE");
	} OP_ENDLINE
	| WRITE ID {
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>2)].lexeme);
		ponerEnPolaca(&polaca,"WRITE");
	}
	OP_ENDLINE
    ;

read: 
	READ ID {
		ponerEnPolaca(&polaca,symbol_table[buscarEnTablaDeSimbolos($<vals>2)].lexeme);
		ponerEnPolaca(&polaca,"READ");
	} OP_ENDLINE
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

condicion: 
		between  { printf("condición con between\n");}
		|
		comparacion
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
	expresion {strcpy(tipoDeDatoComparacion,symbol_table[buscarEnTablaDeSimbolos($<vals>1)].datatype);}operador_comparacion expresion
	;

while:
	WHILE{
			t_info info;
			info.nro=contadorWhile++;
			info.saltoElse=contadorPolaca;
			ponerEnPila(&pilaWhile,&info);
			tipoCondicion=condicionWhile;
			ponerEnPolaca(&polaca,"");
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
			char aux2[20];
			strcpy(aux2, "#");
			strcat(aux2, aux);
			ponerEnPolaca(&polaca, aux2);
			char aux3[20];
			sprintf(aux3, "#%d", topeDePila(&pilaWhile)->saltoElse);
			ponerEnPolacaNro(&polaca, topeDePila(&pilaWhile)->saltoElse, aux3);
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
	PA { isIf=1; } condicion 
	PC 
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
		char aux2[CADENA_MAXIMA];
		strcpy(aux2, "#");
		strcat(aux2, aux);
		ponerEnPolaca(&polaca, aux2);
	}
	bloque_ejecucion
	{
	
		char aux[20];
		sprintf(aux, "%d", contadorPolaca);
		ponerEnPolacaNro(&polaca, topeDePila(&pilaIf)->saltoElse, aux);
		char aux2[20];
		strcpy(aux2, "#");
		strcat(aux2, aux);
		ponerEnPolaca(&polaca, aux2);
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
			break;
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

void guardarPolaca(t_polaca* pp)
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
		ponerEnPolaca(&polacaASM, pn->info.cadena);
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
	crearPila(&pilaASM);

    if ((yyin = fopen(argv[1], "rt")) == NULL) {
        printf("\nNo se puede abrir el archivo de prueba: %s\n", argv[1]);
    } else { 
    	yyparse();
    }
	
    // showSymbolTable(); Función para realizar el debug de la tabla de simbolos.
    crear_TS();
	fclose(yyin);
	guardarPolaca(&polaca);
	printf("\nPOLACA GENERADA\n");	
	generarAssembler(&polacaASM);
	printf("\nASSEMBLER GENERADO\n");
  	return 0;
}

/*Generación de assembler*/
void generarAssembler(t_polaca* pp)
{
	t_nodoPolaca* auxPolaca;
	auxPolaca=*pp;
	int i;
	int nroAuxReal=0;
	int nroAuxEntero=0;
	char aux1[CADENA_MAXIMA]="aux\0";
	char aux2[CADENA_MAXIMA];
	char ultimoTipo[CADENA_MAXIMA]= "none";
	char ultimaCadena[CADENA_MAXIMA];
	int huboAsignacion=TRUE;
	int huboSalto=FALSE;
	pf = fopen("final.asm", "w");

	fprintf(pf, "\nINCLUDE macros2.asm\t\t;Biblioteca\n");
    fprintf(pf, "INCLUDE number.asm\t\t;Biblioteca\n");
    fprintf(pf, "\n.MODEL LARGE\t\t;Modelo de memoria\n");
    fprintf(pf, ".386\t\t;Tipo de procesador\n");
    fprintf(pf, ".STACK 200h\t\t;Bytes en el stack\n");

	fprintf(pf, "\t\n.DATA\t\t;Inicializa el segmento de datos\n");
    fprintf(pf, "\tTRUE equ 1\n");
    fprintf(pf, "\tFALSE equ 0\n");
    fprintf(pf, "\tMAXTEXTSIZE equ %d\n",CADENA_MAXIMA);

	for(i=0; i < table_regs; i++)
    {
		if(strcmp((symbol_table[i]).datatype, "INTEGER") == 0 && atoi((symbol_table[i]).value) == 0 )
		{
			fprintf(pf, "\t_%s dd ?\n",(symbol_table[i]).lexeme);
		}
		if(strcmp((symbol_table[i]).datatype, "FLOAT") == 0 && atoi((symbol_table[i]).value) == 0 )
		{
			fprintf(pf, "\t_%s dd ?\n",(symbol_table[i]).lexeme);
		}
		if(strcmp((symbol_table[i]).datatype, "STRING") == 0 && strcmp(symbol_table[i].value, "") == 0)
		{
			fprintf(pf, "\t_%s db MAXTEXTSIZE dup(?), '$'\n",symbol_table[i].lexeme);
		}
		if(
		(strcmp((symbol_table[i]).datatype, "INTEGER") == 0 
		|| strcmp((symbol_table[i]).datatype, "FLOAT") == 0 ) 
		&& atoi((symbol_table[i]).value) != 0
		) 
		{
			fprintf(pf, "\t_%s dd %s\n",(symbol_table[i]).lexeme, (symbol_table[i]).value);
		}
		if(strcmp((symbol_table[i]).datatype, "STRING") == 0 && strcmp(symbol_table[i].value, "") != 0)
		{
			int longitud = (symbol_table[i]).length;
			int size = CADENA_MAXIMA - longitud;
			fprintf(pf, "\t_%s db %s, '$', %d dup(?)\n", (symbol_table[i]).lexeme, (symbol_table[i]).value, size);
		}
	}

	// AUXILIARES
	for(i=0;i<auxiliaresNecesarios;i++)
	{
		fprintf(pf,"\t_auxR%d \tDD 0.0\n",i);
	}
	for(i=0;i<auxiliaresNecesarios;i++)
	{
		fprintf(pf,"\t_auxE%d \tDW 0\n",i);
	}
	
	// CÓDIGO
	fprintf(pf,"\n.CODE\n.startup\n\tmov AX,@DATA\n\tmov DS,AX\n");

	while(*pp)
    {		
			t_nodoPolaca* auxPolaca = *pp;
			char linea[CADENA_MAXIMA];
			int pos;
			strcpy(linea, auxPolaca->info.cadena);

	    	//VARIABLES
	    	if((pos=buscarEnTablaDeSimbolos(linea))!=ERROR 
			&& (strcmp(symbol_table[pos].value, "") == 0 
			|| atoi((symbol_table[pos]).value) == 0) 
			)
	    	{
	    		t_info info;
	    		info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
	    		strcpy(info.cadena,linea);
	    		info.dataType=symbol_table[pos].datatype;
	    		if(pilaASM ||huboAsignacion==FALSE)
	    		{
	    			huboAsignacion=TRUE;
	    		}
	    		else
	    		{
	    			huboAsignacion=FALSE;
	    		}
	    		ponerEnPila(&pilaASM,&info);
				strcpy(ultimoTipo, info.dataType);
		    }
		
			//CONSTANTES
	    	if((pos=buscarEnTablaDeSimbolos(linea))!=ERROR 
			&& (strcmp(symbol_table[pos].value, "") != 0 
			|| atoi((symbol_table[pos]).value) != 0) 
			)
	    	{
	    		t_info info2;
	    		info2.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
	    		strcpy(info2.cadena,linea);
	    		info2.dataType=symbol_table[pos].datatype;
	    		ponerEnPila(&pilaASM,&info2);
	    		if(strcmp(info2.dataType,"STRING") != 0)
	    		{
		    		if(!pilaASM)
		    		{
		    			huboAsignacion=FALSE;
		    		}
				
		    	}
		    	strcpy(ultimoTipo, info2.dataType);
		    }
		
	    	// operadores aritméticos
		    if(strcmp(linea, "*") == 0 )
		    {
				t_info *op1=sacarDePila(&pilaASM);
				t_info *op2;
				t_info info;
				if(strcmp(op1->dataType, "INTEGER") == 0){
					fprintf(pf,";MULTIPLICACION DE ENTEROS\n");
					op2=sacarDePila(&pilaASM);
					fprintf(pf,"\tfild \t_%s\n", op1->cadena);
					fprintf(pf,"\tfimul \t_%s\n", op2->cadena); 
					strcpy(aux1,"auxE");
					itoa(nroAuxEntero,aux2,10);
					strcat(aux1,aux2);
					fprintf(pf,"\tfistp \t_%s\n", aux1);
					strcpy(info.dataType, "INTEGER");
					info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
					strcpy(info.cadena,aux1);
					ponerEnPila(&pilaASM,&info);
					nroAuxEntero++;
				}else if(strcmp(op1->dataType, "FLOAT") == 0){
						fprintf(pf,";MULTIPLICACION DE REALES\n");
						op2=sacarDePila(&pilaASM);
						fprintf(pf,"\tfld \t_%s\n", op1->cadena);
						fprintf(pf,"\tfld \t_%s\n", op2->cadena);
						fprintf(pf,"\tfmul\n");
						strcpy(aux1,"auxR");
						itoa(nroAuxReal,aux2,10);
			   			strcat(aux1,aux2);
						fprintf(pf,"\tfstp \t_%s\n", aux1);
						strcpy(info.dataType, "FLOAT");
						info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
				    	strcpy(info.cadena,aux1);
				    	ponerEnPila(&pilaASM,&info);
						nroAuxReal++;
				}
			}
	
		    if(strcmp(linea, "+") == 0 )
		    {
				t_info *op1=sacarDePila(&pilaASM);
				t_info *op2;
				t_info info;
				if(strcmp(op1->dataType, "INTEGER") == 0)
				{
					fprintf(pf,";SUMA DE ENTEROS\n");
					op2=sacarDePila(&pilaASM);
					fprintf(pf,"\tfild \t_%s\n", op1->cadena);
					fprintf(pf,"\tfiadd \t_%s\n", op2->cadena); 
					strcpy(aux1,"auxE");
					itoa(nroAuxEntero,aux2,10);
					strcat(aux1,aux2);
					fprintf(pf,"\tfistp \t_%s\n", aux1);
					strcpy(info.dataType, "INTEGER");
					info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
					strcpy(info.cadena,aux1);
					ponerEnPila(&pilaASM,&info);
					nroAuxEntero++;
				}else if(strcmp(op1->dataType, "FLOAT") == 0){
					fprintf(pf,";SUMA DE REALES\n");
					op2=sacarDePila(&pilaASM);
					fprintf(pf,"\tfld \t_%s\n", op1->cadena);
					fprintf(pf,"\tfld \t_%s\n", op2->cadena);
					fprintf(pf,"\tfadd\n");
					strcpy(aux1,"auxR");
					itoa(nroAuxReal,aux2,10);
					strcat(aux1,aux2);
					fprintf(pf,"\tfstp \t_%s\n", aux1);
					strcpy(info.dataType, "FLOAT");
					info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
					strcpy(info.cadena,aux1);
					ponerEnPila(&pilaASM,&info);
					nroAuxReal++;
				}
			}

			if(strcmp(linea, "/") == 0 )
		    {
				t_info *op1=sacarDePila(&pilaASM);
				t_info *op2=sacarDePila(&pilaASM);;
				t_info info;

				if(strcmp(op1->dataType, "INTEGER") == 0)
				{
					fprintf(pf,";DIVISION DE ENTEROS\n");
					fprintf(pf,"\tfild \t_%s\n", op1->cadena);
					fprintf(pf,"\tfidivr \t_%s\n", op2->cadena); 
					strcpy(aux1,"auxE");
					itoa(nroAuxEntero,aux2,10);
					strcat(aux1,aux2);
					fprintf(pf,"\tfistp \t_%s\n", aux1);
					strcpy(info.dataType, "INTEGER");
					info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
					strcpy(info.cadena,aux1);
					ponerEnPila(&pilaASM,&info);
					nroAuxEntero++;
				}else if(strcmp(op1->dataType, "FLOAT") == 0){
					fprintf(pf,";DIVISION DE REALES\n");
					fprintf(pf,"\tfld \t_%s\n", op1->cadena);
					fprintf(pf,"\tfld \t_%s\n", op2->cadena);
					fprintf(pf,"\tfdivr\n");
					strcpy(aux1,"auxR");
					itoa(nroAuxReal,aux2,10);
					strcat(aux1,aux2);
					fprintf(pf,"\tfstp \t_%s\n", aux1);
					strcpy(info.dataType, "FLOAT");
					info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
					strcpy(info.cadena,aux1);
					ponerEnPila(&pilaASM,&info);
					nroAuxReal++;	
				}
			}

			if(strcmp(linea, "-") == 0 )
		    {
				t_info *op1=sacarDePila(&pilaASM);
				t_info *op2=sacarDePila(&pilaASM);;
				t_info info;
				
				if(strcmp(op1->dataType, "INTEGER") == 0)
				{
					fprintf(pf,";RESTA DE ENTEROS\n");
					op2=sacarDePila(&pilaASM);
					fprintf(pf,"\tfild \t_%s\n", op1->cadena);
					fprintf(pf,"\tfisubr \t_%s\n", op2->cadena); 
					strcpy(aux1,"auxE");
					itoa(nroAuxEntero,aux2,10);
					strcat(aux1,aux2);
					fprintf(pf,"\tfistp \t_%s\n", aux1);
					strcpy(info.dataType, "INTEGER");
					info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
					strcpy(info.cadena,aux1);
					ponerEnPila(&pilaASM,&info);
					nroAuxEntero++;
				}else if(strcmp(op1->dataType, "FLOAT") == 0){
					fprintf(pf,";RESTA DE REALES\n");
					op2=sacarDePila(&pilaASM);
					fprintf(pf,"\tfld \t_%s\n", op1->cadena);
					fprintf(pf,"\tfld \t_%s\n", op2->cadena);
					fprintf(pf,"\tfsubr\n");
					strcpy(aux1,"auxR");
					itoa(nroAuxReal,aux2,10);
					strcat(aux1,aux2);
					fprintf(pf,"\tfstp \t_%s\n", aux1);
					strcpy(info.dataType, "FLOAT");
					info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
					strcpy(info.cadena,aux1);
					ponerEnPila(&pilaASM,&info);
					nroAuxReal++;
				}
			}

			// comparadores
			if(strcmp(linea, "CMP") == 0)
			{
				t_info *op1= sacarDePila(&pilaASM);
				t_info *op2= sacarDePila(&pilaASM);
			
				if(strcmp(op1->dataType, "FLOAT") == 0)
				{
					fprintf(pf,"\tfld \t_%s\n", op1->cadena);
					fprintf(pf,"\tfld \t_%s\n", op2->cadena);
				}
				else
				{	
					fprintf(pf,"\tfild \t_%s\n", op1->cadena);
					fprintf(pf,"\tfild \t_%s\n", op2->cadena);
				}/*else if(strcmp(op1->dataType, "STRING") == 0){
					fprintf(pf,"\t_%s\n", op1->cadena);
					fprintf(pf,"\t_%s\n", op2->cadena);
				}*/
			}
		
			if(huboSalto == TRUE){
				fprintf(pf,"\tET_%s\n", linea);
				huboSalto = FALSE;
			}
		
			//>
			if(strcmp(linea, "BLE") == 0)
			{	
				fprintf(pf,"\tfcomp\n\tfstsw\tax\n\tfwait\n\tsahf\n\tjbe");
				huboSalto=TRUE;
			}

			//<
			if(strcmp(linea, "BGE") == 0)
			{
				fprintf(pf,"\tfcomp\n\tfstsw\tax\n\tfwait\n\tsahf\n\tjae");
				huboSalto=TRUE;
			}

			//!=
			if(strcmp(linea, "BEQ") == 0)
			{
				fprintf(pf,"\tfcomp\n\tfstsw\tax\n\tfwait\n\tsahf\n\tje");
				huboSalto=TRUE;
			}

			//==
			if(strcmp(linea, "BNE") == 0)
			{
				fprintf(pf,"\tfcomp\n\tfstsw\tax\n\tfwait\n\tsahf\n\tjne");
				huboSalto=TRUE;
			}

			//>=
			if(strcmp(linea, "BLT") == 0)
			{
				fprintf(pf,"\tfcomp\n\tfstsw\tax\n\tfwait\n\tsahf\n\tjb");
				huboSalto=TRUE;
			}

			//<=
			if(strcmp(linea, "BGT") == 0)
			{
				fprintf(pf,"\tfcomp\n\tfstsw\tax\n\tfwait\n\tsahf\n\tja");
				huboSalto=TRUE;
			}	

			if(strcmp(linea, "BI") == 0)
			{
				fprintf(pf,"\tjmp");
				huboSalto=TRUE;
			}

			// saltos
			if(strcmp(linea, "$") == 0)
			{
				t_info info;
				info.cadena=(char*)malloc(sizeof(char)*CADENA_MAXIMA);
		    	strcpy(info.cadena,(const char*)reemplazarCaracter(linea,"$",""));
		    	ponerEnPila(&pilaASM,&info);
			}

				// etiquetas
			if(strchr(linea, '#') != 0)
			{
				fprintf(pf,"ET_%s:\n",reemplazarCaracter(linea,"#",""));
			}

			// asignación
			if(strcmp(linea, "=") == 0)
			{	
				if(strcmp(ultimoTipo, "STRING") == 0){
					fprintf(pf,";ASIGNACION CADENA\n");
					fprintf(pf,"\tmov ax, @DATA\n\tmov ds, ax\n\tmov es, ax\n");
					fprintf(pf,"\tmov si, OFFSET\t_%s\n", sacarDePila(&pilaASM)->cadena);
					fprintf(pf,"\tmov di, OFFSET\t_%s\n", sacarDePila(&pilaASM)->cadena);
				}else if(strcmp(ultimoTipo, "INTEGER") == 0){
					t_info *op1=sacarDePila(&pilaASM);
					t_info *op2=sacarDePila(&pilaASM);
					fprintf(pf,"\tfild \t_%s\n", op1->cadena);
					if(op2){
						fprintf(pf,"\tfstp \t_%s\n",op2->cadena);
					}
					huboAsignacion=TRUE;
				}else if(strcmp(ultimoTipo, "FLOAT") == 0){
					fprintf(pf,";ASIGNACION FLOAT\n");
					t_info *op1=sacarDePila(&pilaASM);
					t_info *op2=sacarDePila(&pilaASM);
					fprintf(pf,"\tfld \t_%s\n", op1->cadena);
					if(op2){
						fprintf(pf,"\tfstp \t_%s\n",op2->cadena);
					}
					huboAsignacion=TRUE;
				}
			}

			//WRITE
			if(strcmp(linea,"WRITE")==0)
			{
				fprintf(pf,";SALIDA POR CONSOLA\n");
				printf(ultimoTipo);
				if(strcmp(ultimoTipo, "INTEGER") == 0){
					fprintf(pf,"\tdisplayInteger \t_%s,3\n\tnewLine 1\n",sacarDePila(&pilaASM)->cadena);	
				}else if(strcmp(ultimoTipo, "FLOAT") == 0){
					fprintf(pf,"\tdisplayFloat \t_%s,3\n\tnewLine 1\n",sacarDePila(&pilaASM)->cadena);	
				}else if(strcmp(ultimoTipo, "STRING") == 0){
					fprintf(pf,"\tdisplayString \t_%s\n\tnewLine 1\n",sacarDePila(&pilaASM)->cadena);
				}
			}

			//READ
			if(strcmp(linea,"READ")==0)
			{
				fprintf(pf,";ENTRADA POR CONSOLA\n");
				if(strcmp(ultimoTipo, "STRING") == 0){
					fprintf(pf,"\tgetString \t_%s\n",sacarDePila(&pilaASM)->cadena);
				}else{
					fprintf(pf,"\tgetFloat \t_%s\n",sacarDePila(&pilaASM)->cadena);
				}
		    }
		
        	*pp=(*pp)->psig;
		}

		fprintf(pf,"\nmov ax, 4C00h\nint 21h\nend");
		fclose(pf);
    }

