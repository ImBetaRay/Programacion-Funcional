 # ESTRUCTURAS DE CONTROL Y FUNCIONES

 ## Expresiones condicionales

 Al igual que todos los lenguajes usuales Scala cuenta con una estructura if/else, sin embargo en Scala el if/else tiene el valor de la expresión que sigue de la estructura, por ejemplo:

    if (x > 0) 1 else -1

Esta expresión se puede poner en una variable:

    val m = if (x > 0) 1 else -1

Su equivalente en java sería:

    int m;
    if (x > 0){
        m = 1;
    } else
    {
        m = -1;
    }

Hay formas de escribirlo de una forma más optima sin embargo esta es la idea intuitiva, notemos que en scala no son necesarios los puntos y coma. La expresión anterior tambíen es posible en Scala, se vería de la siguiente forma:

    if(x > 0) m = 1 else m = -1

 La primera forma resulta más útil pues puedes iniciar directamente una constante con el valor deseado mientras que en la segunda es necesario que m sea una variable.

Aunque parece revolucionario esta forma de declarar variables existe en java y es aquello que llamamos operador ternario, de cualquier forma la forma de Scala es más avanzada pues permite realizar más cosas dentro del if.

Como sabemos del primer capitulo Scala es constantemente tipado es decir cada valor siempre tendra el mismo tipo, tambíen las expresiones tiene un valor por lo tanto un tipo sin embargo ¿qúe pasa con las siguiente expresión?

    if (x > 0) "Mane" else -1

Para este tipo de expresiones con tipo "contingente" existe el tipo **any** que contiene cualquier tipo existente.

Ahora, ¿qué pasa si omitimos la clausula else? Dado que toda expresión debe tener un valor fue necesario crear la clase **unit** que contiene un solo valor, (). Entonces la expresiones 

    1. if(x > 0) 1
    2. if(x > 0) 1 else ()

Son equivalentes,. La expresión anterior pertenece a la clase o tipo **anyVal**. Podemos pensar en () como un void o null.

_Nota: En Scala no existe la estructura switch pues al ser tambíen un lenguaje funcional cuenta con una herreamienta más poderosa llamada caza de patrones._

## Final de Linea

En Scala el punto y coma no es nunca necesario pues es un lenguaje identado sin embargo se puede usar para declarar explisitamente cuando es que una linea acaba así podemos llegar a evitar errores.

De cualquier forma si se quiere tener varias instrucciones en una linea se usará el punto y coma, por ejemplo:

    if (n > 0) { m = m * l; n -= 1}

Si quieres que una instrucción tenda más de una linea es recomendado que cortes la instrucción en un punto donde no podría terminar sin errores, por ejemplo

    m = m0 + (l - l0) * t +
        0.5 * (a - a0) * t * t

## Bloques de Expresiones y Asignaciones

Usualmente un bloque de expresiones es un monton de lineas encerradas por corchetes {} e igualmente de forma usual los usamos cuando en estructuras de control requerimos de varias lineas para realizar lo deseado.

En Scala, un bloque de {} contiene un conjunto de expresiones cuyo resultado tambien es una expresión. El valor del bloque es la última expresión.

Esto es útil cuando un valor requiere multiplies calculos antes de ser inicializado. Por ejemplo,

    val distancia = { val dx = x - x0; val dy = y - y0; sqrt(dx * dx + dy *dy)}

En scala la asignaciones no tienen valor o hablando estrictamente tienen a () como valor. Por ejemplo, 

    {r = r * n; n -= 1}

tiene un valor de () lo cúal no es un problema sin embargo es útil saberlo cuando se úsan funciones.

De lo anterior se sigue que no podemos concatenar asignaciones de la forma deseada. Por ejemplo,

    x = y = 1

Pese a que en java el valor de x sería 1, en Scala el valor de x es () pues si bien y = 1 el resultado de una asignación es ().

# Entrada y Salida

Al igual que en Java para imprimir usamos las funciones print y println. Print imprime el valor sin un salto de linea despues de imprimirlo, mientras que println si agrega el salto de linea.

Tambien tenemos elementos propios de C como el format string:

    printf("Hello, %s! You are %d years olg.%n", name, age)

O de mejor forma usando interpolación de cadenas

    print(f"Hola, $name! En seís meses, cumplirás ${age + 0.5}%7.2f años.%n")

Los strings con formato contienen siempre el prefijo **f**.  Contienen expresiones que comienzan con **$** y en caso de no ser el nombre de variables es necesario que esten rodeadas por corchetes.

El interpolador f es mejor que usar printf pues es tipado lo cual ayuda a prevenir errores.

Para leer una entrada desde la consola usamos el metodo readLine. Para leer un númerico, booleana o un char usamos readInt, readDouble, readByte, etc. Unicamente el metodo readLine requiere un prompt:

    import scala.io
    val nombre = StdIn.readLine("Your name: ")
    print("Tu edad: ")
    val edad = StdIn.readInt()
    println(s"Hola, ${nombre}! El siguiente año tendrás ${edad + 1}.")

## Ciclos

Scala tiene el mismo while y do que Jav. Por ejemplo 

    while (n > 0){
        m = m * n
        n -= 1
    }

Es valido en scala. Sin embargo Scala no tiene un _for(inicializa, prueba, actualiza)_. En caso de ocuparlo tines dos opciones. Puedes usar un while. O, puedes usar una sentencia for como la siguiente:

    for (i <- 1 to n)
        r = r * i

Notemos que la forma for _(i <- expr)_ hace que la variable **i** se mueva por otros los valores del lado derecho de la expresión.

En scala los ciclos no son tan usados pues mantiene todas las bondades de la programación funcional, es deci tenemos recursión, mapping y otras funciones que cumplen muy bien este proposito.

_Nota: en Scala no existe la sentencia **break** ni **continue**. En caso de requerirlas puedes controlar con if/else o usar return._

## Ciclos Avanzados

Como sabemos en otros lenguajes existen los fors anidados, lo cual se puede hacer de forma usual en Scala, sin embargo existe una forma muy elegante y práctica de hacerlo. 

Se pueden tener multiples generadores de la forma _variable <- expression_. Separados por puntos y comas. Por ejemplo,

    for(i <- 1 to 3; j <- 1 to 3) print(f"${10 * 1 + j}%3d")
    //Imprime 11 12 13 21 22 23 31 32 33

Cada generador puede tener una _guarda_, que es una condición booleana precedida por un if:

    for(i <- 1 to 3; j <- 1 to 3 if i != j) print(f"${10 * 1 + j}%3d")
    //Imprime 12 13 21 23 31 32

Tambíen se pueden tener cualquier número de definiciones para introducir variables ue se usarán dentro del loop:

    for(i <- 1 to 3; from = 4 - i; j <- from to 3) print(f"${10 * 1 + j}%3d")
    //Imprime 13 22 23 31 32 33

Cuando el cuerpo del ciclo empieza con **yield**, el ciclo construye una colección de valores, uno por cada iteración.

    for(i <- 1 to 10) yield i % 3
    //Yields Vector(1,2,0,1,2,0,1,2,0,1)

# Funciones

Scala además de métodos tienen funciones, que a diferencia de los métodos no requieren un objeto. Es parecido a un metodo estatico.

Para definir funciones, escribe el nombre, parametros y el cuerpo, por ejemplo,

    def abs(x: Double) = if (x >= 0) x else -x

Se debe especificar el tipo de cada parametro. Sin embargo mientras la función no sea recursiva no es necesario especificar el tipo de regreso. El compilador de Scala inferira el tipo dada el tipo de expresión despues del igual.

Si el cuerpo de la función requiere más de una linea usa bloques {}. La última expresión del bloque es el valor que la función devolverá. Por ejemplo,

    def fac(n : Int) = {
        var r = 1
        for (i <- 1 to n) r = r * i
        r
    }

Devolverá el valor de r despues del ciclo. No hay necesidad de usar la palabra clave _return_ a menos que se salir de una función inmediatamente.

Para funciones recursivas se especificará el tipo de la siguiente forma,

    def fac(n : Int): Int = if (n <= 0) 1 else n * fac(n-1)

Si no se especifica el tipo a devolver, el compilador no sabrá que el valor a devolver es un int.

# Argumentos por default y Argumentos declarados

En Scala tu puedes poener a las funciones valores por default que son usados cuando no se especifica explicitamente algunos valores. Por ejemplo,

    def decora(str: String, left: String = "[", right: String = "]") = left + str + right

Está función tiene dos parametros con argumentos por default. 

Si llamamos a la función como **decora("HOLA")** te devolverá **"[HOLA]"**. Si no quieres usar los predefinidos, puedes pasar los deseados y se sobreescribirán.

Si das menos argumentos de los requeridos aquello que tengan un valor por default se sustituiran al ejecutar la función, **decora("HOLA", ">>>[")** devolverá **">>>[HOLA]"**

También se pueden pasar los parametros en desorden pero necesitas especificar el nombre del parametro. Por ejemplo,

    decora(left = "<<<", str = "Hola", right = ">>>")

Devolverá **"<<<Hola>>>"**. Adicionalmente, puedes mezclar todo lo anterior. Por ejemplo,

    decora("Hola", right = "]<<<") // Llama a decora("Hola", "[", "]<<<")

## Argumentos variables

Hay veces que queremos que una función no siempre reciba el mismo número de argumentos, para esos casos en Scala tenemos la siguiente sintaxis:

    def sum(args: Int*) = {
        var result = 0
        for (arg <- args) result += arg
        result
    }

Esta función se puede llamar con cualquier cantidad de argumentos.

Si ya tienes una secuencia de valores no puedes pasarlos como argumentos directamente, es necesario decirle al compilador que sea leido como una secuencia de argumentos, para eso añadimos **: _***. Por ejemplo

    val s = sum(1 to 5: _*)

Esta sintaxis es obligatoria en funciones recursivas con argumentos variables.

    def recursiveSum(args: Int*) : Int = {
        if (args.length == 0) 0
        else args.head + recursiveSum(args.tail : _*)
    }

## Valores perezosos

Cuando un valor es declarado como perezoso no se inicializa hasta que es accedido por primera vez. Por ejemplo,

    lazy val words = scala.io.Source.fromFile("/usr/share/dict/words").mkString

Los valores perezosos son utiles pare ahorrar costos de inicialización y para prevenir algúnos errores.

## Excepciones

En Scala las excepciones funcionan igual que en java. Por ejemplo para tirar una tenemos,

    throw new IllegalArgumentException("x should not be negative")

Para atrapar excepciones usamos una sintaxis de caza de patrones, por ejemplo,

    val url = new URL("http://horstmann.com/fred-tiny.gif")
    try {
        process(url)
    } catch {
        case _: MalformedURLException => println(s"Bad URL: $url")
        case ex: IOException => ex.printStackTrace()
    }

Finalmente, la estructura try/catch/finally funciona igual que en java.

    val in = new URL("http://horstmann.com/fred.gif").openStream()
    try {
        process(in)
    } finally {
        in.close()
    }