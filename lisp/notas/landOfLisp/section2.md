# Lisp es Simetria

## Tomando decisiones con condionales

### La simetria de _nil_ y ()

Dado que la filosofia de lisp se centra fuertemente en almacenar y manipular información mediante listas no es de soprender que la estructura de lisp facilite dividir y juntar listas. La decisión más importante en esta filosofia es que lisp tomará la lista vacia "()" con un valor de falso si está envuelta en una oración condicional.

        > (if '()
              'i-am-true
              'i-am-false)
        I-AM-FALSE
        > (if '(1)
              'i-am-true
              'i-am-false)
        I-AM-TRUE

Está decisión resulta peculiarmete útil cuando se trabaja con recursión pues nos permite saber cuando es que alcanzamos el final de la lista de una manera facil.
(Estas funciones que iteran sobre una lista son llamadas _list-eaters_.)

Ejemplo de un list-eater:

        > (defun my-length (list)
                (if list
                    (1+ (my-length (cdr list)))
                    0))
        > (my-length '(lista con 4 elementos))        
        4

## Leyendo e Imprimiendo en Lisp 

### Imprimiendo en Pantalla

La función _print_ imprime lo que se le es pasado como parametro en la consola:

    >(print "foo")
    "foo"
    "foo"

En la consola, _REPL_ apartir de ahora, se muestra dos veces "foo". La primera corresponde a la acción de imprimir. La segunda corresponde al valor de la función evaluada, pues la REPL es interactiva.

La función print agraga un salto de linea al final de cada impresión, para casos donde ese salto sea indeseable tenemos prin1. Por ejemplo,

    >(progn (prin1 "this")
            (prin1 "is")
            (prin1 "a")
            (prin1 "test"))
    "this""is""a""test"

### Leyendo de Terminal

Para leer una entrada del usuario encontramos la función _read_ que espera a que el usuario introduzca algo por la REPL y presione enter. Por ejemplo,

    >(defun di-hola ()
        (print "Por favor escriba su nombre:")
        (let ((nombre (read)))
            (prin1 "Encantado de conocerte,")
            (prin1 nombre)))

En esta función primero imprimimos un mensaje para que se introduzca el nombre, despues usamos la función read para que el usuario introduzca el valor y finalmente imprimimos un saludo y el valor almacenado en la variable.

Como observación la función read y print conservan los tipos y son capaces de inferirlo como es en el caso de read. Por ejemplo:

    >(defun suma-cinco ()
        (print "Ingrese un número:")
        (let ((n (read)))
            (print "Cuando le suamamos cinco queda")
            (print (+ n 5))))

### Imprimiendo y Leyendo como humanos

Si bien la funcón print hace el trabajo las comillas resultan un tanto molestas pues no es la forma intuitiva en la que un humano imprimiría la información. Para resolver lo anterior tenemos la función **princ** que imprime cualquier cosa de la forma más natural posible. Por ejemplo,    

    >(progn (princ "Esta linea se va a cortar")
           (princ #\newline)
           (princ "por un molesto caracter de nueva linea.))
    Esta linea se va a cortar
    por un molesto caracter de nueva linea.

Vemos como las comillas y los carácteres no se imprimen en su forma más cruda por lo que es amigable para los humanos. Sin embargo una vez que se imprime de esta forma se pierde cualquier rastro de que tipo era originalmente.

Siempre es complicado leer información del usuario entonces por practicidad se toma todo lo que el usuario ingresa hasta que presiona enter como una sola cadena, eso en lisp es la función _read-line_. 

    >(defun di-hola ()
        (princ "Por favor ingresa tu nombre:")
        (let ((name (read-line)))
            (princ "Gusto en conocerte, ")
            (princ name)))

Esta función a diferencia de la anterior es capaz de leer un nombre con cualesquiera caracteres, incluyendo espacios y demás y además no imprime las comillas.

### La simetria entre Codigo y Datos en Lisp

Como sabemos de la sección anterior lisp tiene dos modos el de codigo y el de datos, se distinguen pues el de datos comienza con na comilla. Sin embargo esto no se queda ahí LISP puede tratar codigo como datos y viceversa, un lenguaje con esta facilidad se llama **homoiconic**. 

Lo anterior nos permite hacer lo siguiente. Definamos una variable que contiene codigo pero en modo de datos,

    >(defparameter *foo* '(+ 1 2))

Resulta que podemos ejecutar el codigo almacenado en esa variable de la siguiente forma,

    >(eval *foo*)
    3

## Funciones Lambda

Es imposible hablar de un lenguaje funcional sin pasar por las lambdas, despues de todo por ellas es que existen.

### ¿Qué hace una lambda?

En pocas palabras, el comando lambda crea una función sin darle un nombre. Por ejemplo tenemos la siguiente función que toma un número y lo divide a la mitad.

    >(defun half (n)
        (/ n 2))

Así es como lo escribiriamos hasta el momento. Resulta que en Lips, las funciones realmente son valores que podemos ver y pasar como si fuesen números o listas. Un programador experimentado con Lisp diría que las funciones son _valores de primera clase_. Coomo vimos anteriormente, podemos acceder a la función representada por la palabra _half_ usando el operador de función:

    >#'half
    #<FUNCTION HALF ...>

El comando lambda nos permite hacer estas dos cosas en un solo paso. Por ejemplo,

    >(lambda (n) (/ n 2))
    #<FUNCTION :LAMBDA ...>

El primer parametro corresponde a los parametros de la función anonima y el segundo no es más que el cuerpo de la función.

Una vez que tienes tu función lambda se la puedes pasar a alguna función que reciba como parametro a una función como puede ser _mapcar_ o _apply_. Por ejemplo para dividir a la mitad todos los elementos de una lista:

    >(mapcar (lambda (n) (/ n 2)) '(2 4 6))
    (1,2,3)

Las funciones lambda son peculiarmente utiles cuando se programa funcionalmente, crean un nuevo estilo de funciones llamadas funciones de orden superior y permiten muchas cosas que entre más nos adentremos econtreremos lo increibles que son.

## Más allá de las Listas Basicas
 
### Pares

Una forma de construir tuplas de dos elementos (pares) es usar el operador cons con dos elementos. Cons usualmente usado para constuir listas, si recibe como parametros dos elementos en lugar de un elemento y una lista se generara una tupla que sigue siendo tratada como una lista.

    >(cons 2 3)
    (2 . 3)

Es beneficiosa esta estructura pues permite acceder a los elementos de forma constante con los elementos _car_ y _cdr_.

### Listas Circulares

Una lista circular es una lista que en lugar de que su ultimo elemento apunte al vacio este apunta al inicio de la lista.

De forma predeterminada lisp no es tan listo como para entender cuando es que una lista se está autoreferenciando es por eso que requerimos ejecutar el siguiente comando para advertirle.

    (setf *print-circle* t)

Para crear una lista circular (si es que no se quiere definir toda la estructura de datos) se hará de la siguiente forma,

    >(defparameter foo '(1 2 3))
    FOO
    >(setf (cdddr foo) foo)

Lo anterior no hace más que crear una lista infinita que se vería de la siguiente forma '(1 2 3 1 2 3 1 2 3 ...)

### Listas Asociativas

Son listas que se componen de la estructura llave valor (las usamos en el [textGame](lisp/ejercicios/landOfLisp/textGame)). Son parecidas a los diccionarios pero sin la beneficiosa velocidad de consulta.

Se ven de la siguiente forma:

    (defparameter *drink-order* '(bill . double-expresso)
                                 (lisa . small-drip-coffee)
                                 john . medium-latte)

## Tipos Avanzados y Genericos

### Array

El array común es muy similar a una lista. La ventaja del array sobrea la lista es que el tiempo de consulta es constante.

Para crear un array se usa el comando __make-array__, especificando el tamaño del array:

    >(make-array 3)
    #(NIL NIL NIL)

El ejemplo anterior crea un arreglo de tamaño 3. Para indicar que no es un lista, Common Lisp añade un **#** al principio del arreglo.

Para obtener y modificar elementos en un arreglo usamos la función __aref__. Por ejemplo, para obtener el elemento en el indice 1:

    >(defparameter x (make-array 3))
    #(NIL NIL NIL)
    >(aref x 1)
    NIL

aref solo es suficiente para acceder a elementos, si se queiren modificar es necesario componer aref con __setf__:

    >(setf (aref x 1) 'foo)
    FOO
    >x
    #(NIL FOO NIL)

_Notemos que el hecho de que podamos usar la función __setf__ tanto en lista como en arreglos nos da na pista de que podemos programar de forma general._

Veamos un ejemplo de lo anterior con listas y arreglos.
Tenemos el siguiente programa con listas:

    >(setf foo '(a b c))
    (A B C)
    >(second foo)
    B
    >(setf (second foo) 'z)
    Z
    >foo
    (A Z C)

Ahora veamos como _setf_ es generico y da lugar a cosas complejas:

    >(setf foo (make-array 4))
    #(NIL NIL NIL NIL)
    >(setf (aref foo 2) '(x y z))
    (X Y Z)
    >foo
    #(NIL NIL (X Y Z) NIL)
    >(setf (car (aref foo 2)) (make-hash-table))
    #S(HASH-TABLE)
    >(setf (getHash 'zoink (car (aref foo 2))) 5)
    5
    >foo
    #(NIL NIL (#S(HASH-TABLE (ZOINK . 5)) Y Z ) NIL)

Parece que existiendo las listas los arreglos carecen de sentido, pero esto es mentira pues las listas al estar hechas de celdas compuestas por _cons_ para acceder a un elemento en especifico se tiene que consultar una a una las celdas cosa que es poco practica si tomamos millones de datos. Para los arreglos el calculo anterior toma lo que toma hacer algunos calculos matematicos pues tiempo de acceso es constante dado un polinomio llamado _polinomio de direccionamiento_.

### Tablas HASH

Como vimos anteriormente las tablas Hash son similares a las listas asociativas excepto que los Hash son más rapidas al consultar elementos arbitrarios. 

Las tablas hash son tan rapidas que a veces parecen magias, puede meter tu tabla en una computadora de los 80 y se calculará enseguida.

Para crear una tabla usamos el comando `make-hash-table`:

    >(make-hash-table)
    #S(HASH-TABLE ...)

Como las AList las tablas hash guardan elementos usando el metodo *llave/valor*. Para obtener un valor usamos la funcion `gethash`. Y al igual que con los arreglos, para modifica/añadir un elemento usamos la composición de gethash con setf:

    >(defparameter x (make-hash-table))
    #S(HASH-TABLE ...)
    >(setf (gethash 'yup x) '25)
    >(gethash 'yup x)
    25 ;
    T

__El segundo valor devuelto corresponde a si es que se encontró el valor o no habia coincidencias.__

Notemos que aquí tambien se nos da una pista de que las funciones en lisp pueden devolver más de un valor.


## Estructuras en Common Lisp

Las estructuras pueden ser usadas para representar objetos con propiedades, como es usual de los lenguajes orientados a objetos. Para construir una estructura usamos el comando defstruct:

    >(defstruct person
                name
                age
                waist-size
                favorite-color)
    PERSON

Según la definición anterior, una persona tiene cuatro propiedades (tambien llamadas slots por los expertos de Lisp): name, age, waist-size y favorite-color.   

 Ahora podemos instanciar nuestro "objeto" con la funcion make-person que Lisp amablemente crea cuando definimos la estructura.

    >(defparameter *bob* (make-person :name "Bob"
                                      :age 35
                                      :waist-size 32
                                      :favorite-color "blue"))
    *BOB*

Ahora podemos ver a Bob en la terminal marcada como una estructura por el prefijo #S.

    >*bob*
    #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")

Ahora notemos que Lisp crea un metodo para acceder a cada "atributo" compuesto de la siguiente forma `structura-atributo`. Ahora componiendo esta función y setf podemos modificar nuestra estructura:

    >(setf (person-age *bob*) 36)
    36
