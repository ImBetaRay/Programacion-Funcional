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

