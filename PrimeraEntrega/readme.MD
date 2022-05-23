# Plantilla para TP Integrador usando Flex y Bison (C)

## Prerequisitos. 

Para poder usar esta plantilla deberá:

1. Instalar [flex](https://github.com/westes/flex)

2. Instalar [bison](https://www.gnu.org/software/bison/)

3. Instalar [gcc](https://gcc.gnu.org/)


### Windows. 

Descargue los instaladores para Windows de las siguientes páginas:

1. [Flex](http://gnuwin32.sourceforge.net/packages/flex.htm)
2. [Bison](http://gnuwin32.sourceforge.net/packages/bison.htm)
3. [GCC](https://sourceforge.net/projects/mingw/files/)
4. Siga el siguiente [instructivo](https://issuu.com/lilianachisaguano/docs/instalacion_de_flex__bison_y_mingw)

* Tanto para los binarios instalados flex y bison, es recomendable instalarlos en C:\GnuWin32 y luego agregar "C:\GnuWin32\bin" en variables de entorno -> variables del sistema -> editar variable path y agregar a la lista.
* Lo mismo para gcc, agregar el path donde sea instalado (ej: C:\MinGW\bin) a las variables del sistema en la variable path.

```sudo apt-get update
brew install flex
brew install bison
brew install gcc
```
3. Verifique la instalación ha sido exitosa ejecutándo los siguientes comandos

```
which flex
which bison
which gcc
```
# Requisitos para el grupo 12

## Temas comunes

### ITERACIONES
```
Implementación de ciclo WHILE
```
### DECISIONES
```
Implementación de IF
```
### ASIGNACIONES
```
Asignaciones simples A:=B
```
### TIPO DE DATOS
#### Constantes numéricas
 ```
reales (32 bits)
enteras (16 bits)
```
#### El separador decimal será el punto “.”
```
Ejemplo:

a = 99999.99
a = 99.
a = .9999
```
#### Constantes string
```
Constantes de 30 caracteres alfanuméricos como máximo, limitada por comillas (“ “) ,de la forma “XXXX”

Ejemplo:

b = “@sdADaSjfla%dfg”
b = “asldk  fh sjf”
```

### VARIABLES

#### Variables numéricas
```
Estas variables reciben valores numéricos tales como constantes numéricas, variables numéricas u operaciones que arrojen un valor numérico, del lado derecho de una asignación.

Las variables no guardan su valor en tabla de símbolos.
Las asignaciones deben ser permitidas, solo en los casos en los que los tipos son compatibles, caso contrario deberá desplegarse un error.
```

### COMENTARIOS
```
Deberán estar delimitados por “-/” y “/-” y podrán estar anidados en un solo nivel.

Ejemplo1:
 -/ Realizo una selección /-			
	IF (a <= 30) 
		b = ”correcto” -/ asignación string /-
	ENDIF

Ejemplo2:  

-/ Así son los comentarios de LyC/-

Los comentarios se ignoran, de manera que no generan un componente léxico o token
```
### NTRADA Y SALIDA
```
Las salidas y entradas por teclado se implementarán como se muestra en el siguiente ejemplo:

Ejemplo:

WRITE “ewr” -/ donde “ewr” debe ser una cte string /-
READ base      -/ donde base es una variable /-
WRITE var1 -/ donde var1 es una variable numérica definida previamente /-
```
### CONDICIONES
```
Las condiciones para un constructor de ciclos o de selección pueden ser simples ( a < b )  o múltiples.
Las condiciones múltiples pueden ser hasta dos condiciones simples ligadas a través del operador lógico (AND, OR) o una condición simple con el operador lógico NOT
```
### DECLARACIONES
```
Todas las variables deberán ser declaradas dentro de un bloque especial para ese fin, delimitado por las palabras reservadas DECVAR y ENDDEC, siguiendo el siguiente formato: 
```
### DECVAR
```
Línea_de_Declaración_de_Tipos 
	ENDDEC

 Cada Línea_de_Declaración_de_Tipos tendrá la forma: < Lista de Variables> :  Tipo de Dato 

 La Lista de Variables debe ser una lista de variables separadas por comas.
Pueden existir varias líneas de declaración de tipos, incluso utilizando más de una línea para el mismo tipo.

Ejemplos de formato:    

DECVAR
a1, b1 : FLOAT
variable1 : STRING
		p1, p2, p3 : FLOAT
ENDDEC
```
## Temas especiales
* BETWEEN
* AVG

## GCI
* POLACA INVERSA

## PASOS PARA GENERAR EL EJECUTABLE
1. Generar el sintáctico .y
```
bison -d Sintactico.y
```
2. Incluir el archivo generado Sintactico.tab.h en el Lexico.l
3. Compilar con flex
```
FLEX lexico.l
```
4. Generar ejecutable
```
gcc lex.yy.c
```
5. Usar ejecutable contra el archivo de prueba
```
a.exe prueba.txt
```
