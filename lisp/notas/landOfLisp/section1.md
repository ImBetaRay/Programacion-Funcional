# Lisp es Poder

## Sintaxis

- Para llamar una función en lisp se ponen parentesis alrededor de esta con cualesquiera parametros deseados por el usuario.

- Las variables globales en lisp son conocidas como _top-level definition_, se crean con la función _defparameter_ por ejemplo:
        
        > (defparameter *small* 1)
        *SMALL*
        
El primer argumento es el nombre de la variable, los asteriscos que rodean a la función son llamados _earmuffs_ son opcionales aunque es una buena practica usarlos para distinguir variables globales. 

_Nota: Cualquier valor anteriormente almacenado en la variable nombrada con el parametro pasado a __defparameter__ será sobreescrito, para solucionar esto se puede usar la función __defvar__ que recibe los mismos parametros que __defparameter__ sin embargo el valor solo se asignará si el registro de la variable se encuentra vacio_

- Las funciónes en lisp se definen usando la función defun que recibe el nombre de la función despues sus parametros encerrados por un parentesis y finalmente todo el codigo que compondrá a la función, pore ejemplo:

        > (defun function_name (arguments) ...) 
        > (defun guess-my-number () 
            (ash (+ *small* *big*) -1))

- Las variables locales se definen usando let, por ejempo 
        
        > (let (variable declarations) ...body...)
        > (let ((a 5) (b 6)) (+ a b))

### Tipos

- Symbols: 
        
Un simbolo en lisp es una cadena unitaria, usualmente los simbolos están compuestos por letras, números y carácteres como __+ - / * = < > ? ! _.__. Los simbolos en lisp NO son sensibles a las mayusculas (aunque los usuarios de lisp evitan usar mayusculas).  

- Números:

Lisp incluye tanto enteros como flotantes, para diferenciar un entero de un flotante basta con incluir el punto decimal. En lisp 1 y 1.0 son distintos.

Si un operador es aplicado a un entero y a un flotante la función devolverá un flotante. Lisp parece no tener limite en cuanto a la capacidad de almacenamiento de los números por ejemplo lisp es capaz de ejecutar (exp 53 53). 

__En lisp la división con dos enteros devuelve la fracción reducida por ejemplo:__ 

        >(/ 4 6)
        2/3

Para el caso de haber algún flotante regresa la división expresado como un decimal.

- Strings:

Igual que en cualquier lenguaje son cadenas rodeadas por comillas, en lisp tambien las cadenas pueden incluir carácteres de escape, para incluir comillas o carácteres reservados al igual que en latex hay que incluir una barra antes de este carácter.

### Modos en lisp.

La consola de lisp tiene dos modos el de code que ejecuta las funciones que se dan en la terminal y el modo de datos que lee cualquier cosa como listas de información este segundo modo no es el predeterminado para acceder hay que comenzar lo que sea que queramos tratar como información con una comilla.

## Listas

Las listas probablemente sean el elemento más fundamental de lisp pues como se ha visto anteriormente todo es una lista como lo indican los parentesis.

Las listas en lisp están constituidas por "cons cells".

Una cons cell está compuesta por dos apuntadores, ya sea a un dato o a otra cons cell. Es decir todos son listas simplemente ligadas. 

Para manipular listas en lisp al igual que en haskell se ocupa la función __cons__ que concatena un elemento con una lista aunque en lisp puede ser tambien un elemento.

La función __car__ devuelve el primer elemento de una lista.

La función __cdr__ devuelve el resto de la lista.

Para facilitar la creación de listas existe la función __list__ que recibe como argumentos todos los elementos que compondrán a la lista.

 

