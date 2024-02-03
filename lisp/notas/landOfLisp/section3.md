# LISP ES HACKING

## Ciclando con el comando LOOP

### El Macro loop

Cualquier ciclo que pueda desearse en un programa puede ser conseguido con el `loop` macro. Aquí un ejemplo:

    > (loop for i
            below 5
            sum i)
    10

Este suma los números positivos menores a 5, como

    0 + 1 + 2 + 3 + 4 = 10

Vemos que este ciclo no se comporta como usualmente lo 
hace un programa de lisp.

+ `for` te permite declarar una variable que itera a traves de un rango de valores. Por defecto, contará los enteros empezando por cero.

+ `below` indica que hay que parar una vez alcanzando cierto valor, excluyendo al valor mismo.

### Trucos para el loop

Se pueden usar las clausulas `from` y `to` para hacer un ciclo for que cuentra entre un rango especifico de enteros:

    > (loop for i
            from 5
            to 10
            sum i)
    45

Tambíen se puede iterar sobre valores de una lista usando `in`:

    > (loop for i
            in '(100 20 3)
            sum i)
    123

Se pueden hacer más cosas que sumar, para ello usamos la clausula `do`:
    
    > (loop for i
            below 5
            do (print i))
    0
    1
    2
    3
    4

Tambíen se puede solo realizar acciones cuando la variable (usualmente i) cumple cierta propiedad:

    > (loop for i
            below 10
          when(oddp i)
          aum i)
    25

Podemos juntar todo lo anterior para el siguiente loop:

    > (loop for i
            from 0
           do (print i)
           when (= i 5)
           return 'falafel)
    0
    1
    2
    3
    4
    5

    FALAFEL

Notemos que no hay algo que le diga cuando detenerse más que el usual return, de no estar nunca se detendria.

Se pueden regresar multiples elementos usando la clausula `collect`. Por ejemplo:

    > (loop for  i
            in '(2 3 4 5 6)
            collect (* i i))
    (4 9 16 25 36)

Es posible iterar sobre dos elementos al mismo tiempo, para esto basta con usar un for 2 veces, que a diferencia de un anidado incrementa el valor al mismo tiempo. Por ejemplo:

    > (loop for x below 10
            for y below 10
            collect (+ x y))
    (0 2 4 6 8 10 12 14 16 18)

Tambíen se puede hacer algo equivalente a los for anidados:

    > (loop for x below 10
        collect (loop for y below 10
                      collect (+ x y)))
    ((0 1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9 10) (2 3 4 5 6 7 8 9 10 11) (3 4 5 6 7 8 9 10 11 12) (4 5 6 7 8 9 10 11 12 13) (5 6 7 8 9 10 11 12 13 14) (6 7 8 9 10 11 12 13 14 15) (7 8 9 10 11 12 13 14 15 16) (8 9 10 11 12 13 14 15 16 17) (9 10 11 12 13 14 15 16 17 18))

## Imprimiendo con la función Format

La función `format` en uso se ve de la siguiente forma:

    > (format t "Add onion rings for only ~$ dollars more!" 1.5)
    Add onion rings for only 1.50 dollars more!
    NIl

Veamos que hace cada cosa

      t ; Indica el valor de destino.
        ; nil = crea un string
        ; t = imprime en consola
        ; stream = mandarlo a un stream

    "Add onion rings for only ~$ dollars more!"
        ; Es el string con formato.

    ~$  ; Es una secuencia modificable

    1.5 ; Un parametro.
 
 ## Trabajando con Streams

 Los flujos son tipos de datos en Common Lisp que permiten tomar recursos externos y hacerlos pasar como otra pieza de información que se puede usar en codigo.

 Los recursos externos pueden ser: archivos en un disco, otra computadora en la red, o texto en la consola.

 ### Tipos de Streams 

 Cuando nos comunicamos con un recurso externo desde un programa de Common Lisp, lo hacemos usando un stream. Hay diferentes tipos de Streams dependiendo del tipo de recurso. De igual forma varían según si se requiere leer o escribir del recurso.

 Si los organizamos por tipo de recurso con el que operan, los siguientes son los más comúnes:

- *Console streams* Los que se usan cuando nos comunicamos por la termina.
- *File streams* Nos permite leer y escribir archivos de nuestro disco.
- *Socket streams* Nos permite comunicar con otras computadoras en una red.
- *String streams* Nos permite leer y recibir texto de un Lisp string.

Si los organizmos por dirección tenemos que cuando escribimos, usamos *output streams*. Para leer usamos *input streams*.

- **Output Streams** son usados para tareas como        escribir en la terminal, escribir en un archivo, o enviar información a traves de un socket. En el nivel más primitivo podemos hacer dos cosas:

    1. Confirmar si el flujo es valido.
    2. Añadir un nuevo elemento al stream.

 Para ver si tenemos un output stream valido, usamos la función output-stream-p. Por ejemplo, la REPL de lisp tiene un output stream asociado con ella llamado `*standard-output*`. Podemos que es valido con el siguiente codigo:

    > (output-stream-p *standard-output*)
    T

Un Lisp character es un elemento que puede ser añadido a un output stream usando el comando basico `write-char`. POr ejemplo, para escribir el carácter #\x en el `standart-output` stream, usamos el siguiente comando:

    > (write-char #\x *standard-output*)
    x

- **Input streams** Son usados para leer información. Así como con los output streams, las acciones posibles son limitadas. Al nivel más primitivo podemos hacer dos cosas:

    1. Checar si el flujo es valido.
    2. Eliminar un elemento del stream.

Podemos ver si tenemos un flujo valido con el comando `input-stream-p`. Por ejemplo la REPL tambíen tiene un imput stream asociado llamado *standard-input*, el cual podemos validar de la siguiente forma:

    > (input-stream-p *standard-input*)
    T

Podemos eliminar un elemento del stream con el comando `read-char`. Como estamos leyendo de la REPL, necesitamos escribir algunas cosas y presionar enter para enviar la información por la entrada standard:

    > (read-char *standard-input*)
    123
    #\1

### Trabajando con archivos

Se pueden crear file streams de muchas formas. La mejor forma es usando el comando `with-open-file`. Este comando contiene medidas especiales contra bugs que lo hacen más seguro de usar que otros comandos. 

    >(with-open-file (my-stream "data.txt" :direction :output)
        (print "my data" my-stream))

Escribimos la cadena "my data" a un archivo llamado data.txt.`with-open-file` une el output stream con el nombre *my-stream*. Este stream unicamente estará disponible dentro del cuerpo de `with-open-file`.

Especificar :output como la dirección para `with-open-file` crea un output strea. Para hacerlo un imput stream, cambiamos la dirección a :input, como:

    >(with-open-file (my-stream "data.txt" :direction :input)
        (read my-stream))
    "my data"

El comando `with-open-file` puede tomar varias palabras clave como parametros para modificar su comportamiento. Por ejemplo, puedes decir que hacer si un archivo con el mismo nombre ya existe. 

    >(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
        (print "my data" my-stream))
    *** - OPEN: file #p".../data.txt" already exists
    >(with-open-file (my-stream "data.txt" :direction :output 
                                            :if-exists :supersede)
        (print "my data" my-stream))
    "my data"

### Trabajando con Sockets

Un socket es un mecanismo para enrutar datos a través de una red entre programas ejecutándose en diferentes computadoras en esa red.

Desafortunadamente, los sockets no están dentro del standar para ANSI Common Lisp, lo que significa que no hay forma generica para interactuar con sockets. De cualquier forma cada versión de lisp soporta sockets. Como hemos estado usando CLISP usaremos los comandos de sockets de este.

- **Socket Addressses** Cada socket en una red debe tener su dirección. Esta dirección tiene dos componenetes: 

    1. IP adrress. Un número que identifica de forma unica a la computadora.
    2. Port number. Cualquier programa que quiera usar la red debe escoger un único port number que ningún otro programa en la misma computadora ya esté usando.

Estos dos números se combinar para crear la dirección del socket. Cualquier mensaje corriendo en una red será nombrado con una socket adress para indicar su destino.

Una vez que una computadora recibe un paquete con su dirección IP, esta mirara el port number para saber que programa debe recibir ese mensaje.

- **Socket Connections** Para realmente enviar un mensaje a través de un socket entre dos programas, necesitamos seguir unos pasos para inicializar la conexión. El primer paso es que uno de los programas cree un socket que inicia en un estado de escucha, esperando si otro programa quiere iniciar comunicación. La computadora con este socket es llamada *server*. Despues el otro programa, llamdo *cliente*, crea un socket y lo usa para establecer conexión con el server. Si todo sale bien, ambos programas deberian transmitir mensajes a través de la conexión entre ellos.


- **Enviando un Mensaje a traves de un Socket**

Primero abrimos dos copias de CLISP, en dos ventanas distintas. Una será el server y otra el cliente.

En el server, tomamos control de un puerto llamando a `socket-server`:

    > (defparameter my-socket (socket-server 4321)) ;ON THE SERVER
    MY-SOCKET

Este comando es de alguna forma peligroso pues el sistema espera que dejemos el socket una vez que terminemos. Si no lo hacemos no seremos capaces de volver a usar el socket, hasta el proximo reinicio.

Ahora hagamos un stream de este socket, que maneje la conexión de un solo cliente:

    > (defparameter my-stream (socket-accept my-socket))

Despues de esto la REPL se bloqueara hasta que una conexion se realice.

Ahora del lado del cliente, usamos el comando `socket-connect` para conectarnos al server.

    > (defparameter my-stream (socket-connect 4321 "127.0.0.1")) 

La dirección IP *127.0.0.1* es especial pues siempre apunta a la computadora de donde es llamada. Una vez ejecutemos esto en el cliente, la REPL del server se desbloqueará y la variabla se colocara. Ahora podemos comunicarnos entre ambas ventanas.

El stream que CLISP creo es bidireccional es decir puede leer y escribir. Vamos a saludar.

Escribimos lo siguiente en el cliente:

    > (print "k tranza server!" my-stream)
    "k tranza server!"

Y en el server escribimos:

    > (read my-stream)
    "k tranza server!"

Finalmente es necesario que cerremos ambos streams y liberemos el socket. Primero ejecutamos el siguiente comando en cliente y servidor para cerrar el stream en ambos extremos:

    > (close my-stream)
    T

Despues ejecutamos `socket-server-close` en el servidor para liberar el puerto y desconectar al socket de ahi.

    > (socket-server-close my-socket)
    NIL

## Manejo de Errores

### Señalizando una Condición.

Si se escribe una función y algo sale terriblemente mal, una función puede notificar al ambiente de Lisp que un problema fue encontrado. Lo anterior se hace con la señalización.

Si se quiere señalizar una condición directamente, se puedce hacer con el comando `error`. Esto se hace si una función detecto el error por si misma, un problema es que la función no podria continuar de forma normal.
Usar el comando error provocará una interrupción del programa a menos que se intercepte el error en algún otro lugar.

Señalemos una condición e imprimamos el mensaje "chin" para describir el error:

    > (error "chin")

    *** - chin
    The following restarts are available:
    ABORT   :R1     Abort main loop

En la mayoria de situaciones cuando tratemos de manejar errores, no será porque llamamos al error sino porque nuestro programa tiene un bug o porque llamamos a una función que ya maneja errores.

Una forma más sofisticada de señalizar una condición es primero definir una condición personalizada usando `define-condition`, como en el ejemplo:

    (define-condition chin ()()
        (:report (lambda (condition stream)
                    (princ "Para de estar CHINgando!" stream))))

Este es un ejemplo clasico de crear un nuevo tipo de condición, la cual llamamos `chin`. Cuando esta condición es señalizada, podemos añadir una función personalizada que será llamada para reportar el error. 

### Interceptando condiciones

Podemos manejar errores y realizar algo cuando un error es invocado. Para esto usamos el comando `handler-case`, por ejemplo:

    > (defun bad-function ()
        (errror 'chin))
    BAD-FUNCTION
    > (handler-case (bad-function)
            (chin () "somebody signaled chin!")
            (bar () "somebody signaled bar!"))
    "somebody signaled chin!"