# TUPLAS Y HASH MAPS    

## Construyendo un Hash Map

Los hash maps, o Diccionarios como se suelen conocer, son estrcuturas de datos construidas por pares de la forma _llave/valor_.

En Scala pauedes construir un diccionario de la siguiente forma

    val calificaciones = Map("Alice" -> 10, "Bob" -> 3, "Cindy" -> 8)

La forma anterior crea un __Map[String, Int]__ inmutable, es decir sus valores no pueden ser alterados.
En caso de requerir alterar valores en nuestro diccionario ocupamos

    val calif = scala.collection.mutable.Map("Alice" -> 10,  "Bob" -> 3, "Cindy" -> 8)

En Scala como se menciono anteriormente un Diccionario es un conjunto de pares. Un par son dos valores agrupados, estos valores no son obligatoriamente del mimso tipo.

En Scala el operador *->* crea un par. Por ejemplo

    "Alice" -> 10

Es 
    
    ("Alice", 10)

Por lo tanto nuestro diccionario original podria haber sido construido como

    val calificaciones = Map(("Alice", 10), ("Bob", 3), ("Cindy", 8))

La primera notación resulta más intuitiva y no tienes que lidiar con paréntesis además de que refuerza la idea de que un map es aplicarle una función de dispersión a un valor, lo anterior lo veremos en el futuro.

## Accediendo a los Valores

En scala para acceder a los valores de un diccionario podemos usar el valor de la siguiente forma

    val calificacionBob = calificaciones("Bob") // Se evalua en 3

En caso de no existir ese valor se manda un error.
Para saber si existe un valor podemos usar el metodo _contains_:

    val califBob = if (calificaciones.contains("Bob")) calificaciones("Bob") else 0

Dado que esto ultimo es muy frecuente de hacer y hacerlo de esa forma ocupa mucho tiempo existe un acortador:

    val valifBob = calificaciones.getOrElse("Bob", 0)

## Actualizando Valores

En un diccionario mutable se pueden cambiar los valores o añadir nuevos, esto se hace con un () a la izquierda del signo =. Por ejemplo,

    calif("Bob") = 10
        //Actualizará el valor existinte a 10
    
    calif("Fred") = 7
        //Añadira un nuevo par a las calificaciones.

Para añadir multiples elementos a la vez usamos lo siguiente

    calif ++= Map("Bob" -> 10, "Fred" -> 7)

Para borrar usamos el operador _-=_:

    calif -= "Alice"

En caso de tener un diccionario inmutable lo que se puede hacer es obtener un nuevo valor con el diccionario deceado.

    val newCalifs = calificaciones ++ Map("Bob" -> 10, "Fred" -> 7)

Claro, en lugar de guardar en un nuevo valor se puede declarar inicialmente el diccionario como una variable.

## Iterando sobre Diccionarios

    for((k,v) <- map) process k and v

Sorprendentemente el for anterior itera sobre cada pareja de valores en el diccionario. La magia es que se puede usar caza de patrones para un Ciclo For en Scala. De esta forma puedes obtener la llava y el valor sin mucho problema.

Si solo se quiere consultar el valor o la llave tenemos los metodos __keySet__ y __values__. Adicionalmente el metodo _values_ devuelve un iterable que se puede usar en un ciclo for.

    calificaciones.keySet //Devuelve los nombres
    for (v <- calificaciones.values) println(v)
        //Imprime las calificaciones

El diccionario reverso cambia los valores a llaves y viceversa. Para crearlo usamos

    for ((k,v) <- map) yield (v, k)



