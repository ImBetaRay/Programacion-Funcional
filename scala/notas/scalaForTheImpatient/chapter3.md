# TRABAJANDO CON ARREGLOS

## Arreglos de Tamaño Fijo

Al igual que en Java en Scala existen arreglos de tamaño fijo, son de tipo _Array_. Por ejemplo,

    val nums = new Array[Int](10) 
    //Arreglo para 10 enteros inicializados en 0.
    val strA = Array("Hola", "Mundo")
    //Array de strings de tamaño 2.
    //Notemos que cuando hay valores iniciales no se usa el new.
    s(0) = "Adios"
    //Se usan () en lugar de [] para acceder a elementos.

## Array de Tamaño Dinamico

Java tiene el conodido _ArrayList_ para arreglos de tamaño dinámico. El equivalente en Scala es el **ArrayBuffer**. Se usa de la siguiente manera,

    import scala.collection.mutable.ArrayBuffer
    val b = ArrayBuffer[Int]()
        // Tambíen es valido "val b = new ArrayBuffer[Int]"
    b += 1
        // Añade un elemeto al final.
    b += (1,2,3)
        // Se añaden multiples elementos encerrandolos con parentesis.
    b ++= Array(8,13,21)
        //Se  concatenen colecciones con el operador ++=
    b.dropRightInPlace(5)
        //Remueve los ultimos 5 elementos. 

Añadir elementos o borrarlos toma un tiempo casi constante para los ArrayBuffer.

Tambien se pueden añadir en cualquier lugar o borrar en cualquier posición pero toman tiempo lineal lo cual es menos eficiente. Por ejemplo,

    b.insert(2,6)
        //Inserta en la posición 2 (contando desde 0) el valor 6
    b.remove(2)
        //Remueve el elemento en la posición 2
    b.remove(2,3)
        //Remueve desde la posición 2 los 3 elementos siguientes.
    
Como probablemente sea necesario alguna vez es posible convertir Array en ArrayBuffer y viceversa.

    b.toArray
    a.toBuffer  
    
## Recorrer Arreglos

En java hay muchas y distintas formas de recorrer arreglos y arrayList, usualmente distintas entre arreglos y arrayList. En Scala es más uniforme, casi siempre usas la misma forma para ambas estructuras.

Para iterar un array o arrayBuffer en scala usamos un ciclo for:

    for (i <- 0 until a.length)
        println(s"$i: ${a(i)}")

El metodo until es similar al meodo to visto en el capitulo anterior, excepto que excluye el ultimo valor. Por lo tanto la variable i va de 0 a a.length -1.

Generalizando tenemos:

    for (i <- range)

Que itera por cada valor en el rango.

Como ejemplo más complejo tenemos que queremos visitar cada 2 elemento es decir los pares.

    0 until a.length by 2
        //Range(0,2,4,...)

De lo anterior podemos notar que podemos recorrer un arreglo de reversa.

    0 until a.length by -1
        //Range(...,2,1,0)

En caso de no ocupar el indice puedes iterar directamente por elemento.

    for (elem <- a)
        println(elem)

## Transformando Arrays

En Scala dado que cada expresión tiene su valor propio podemos transformar arrays sin modificar la estructura original. Por ejemplo,

    val a = Array(2,3,5,7,11)
    val result = for (elem <- a) yield 2 * elem
        //Devuelve Array(4,6,10,14,22)

El ciclo for/yield crea una nueva estructura del mismo tipo que la original.

De igual forma que en el capitulo anterior podemos crear guardas para solo iterar sobre elementos que cumplen una condición.

    for (elem <- a if elem % 2 == 0) yield 2 * elem
        //Solo afecta a los pares Array(4,3,5,7,11)

## Algoritmos útiles

1. Hay veces que deseamos sumar todos los elementos de un arreglo.

        Array(1,7,2,9).sum
            //19, tambíen funciona para Arraybuffer
            //Para usarse el Array debe de ser de tipo númerico.

2. Siempre es bueno ordenar un arreglo.

        val b = ArrayBuffer(1,7,2,9)
        val bSorted = b.sorted
            //b se mantiene igual.
            //bSorted es ArrayBuffer(1,2,7,9)

3. Se puede ordenar de otras formas pero para eso se usa el metodo _sortWith_.

        val bDescending = b.sortWith( _ > _ ) 
            //ArrayBuffer(9,7,2,1)

4. En caso de querer ordenar el arreglo modificando directamente el arreglo original tenemos el metodo _quickSort_ sin embargo este solo funciona para Arrays.

        val a = Array(1,7,2,9)
        scala.util.Sorting.quickSort(a)
            //a ahora es Array(1,2,7,9)

5. Si se quiere obtener el minimo o el maximo tenemos las funciones _min_ o _max_.

        ArrayBuffer("Mary", "had", "a", "little", "lamb").max
            // "little"

_Nota: Para los metodos, min, max y quickSort la clase de los elementos del arreglo debe tener un metodo de comparación._

6. Por ultimo pero no menos importante si se quiere imprimir tenemos el metodo **mkString**. Que puede recibir uno o tres elementos. Cuando se recibe uno es lo que se pondra como separación de los elementos. Cuando recibe 3, el primero y ultimo son el prefijo y el sufijo respectivamente y el de en medio es con el que se separarán los elementos.

        a.mkString(" and ")
            // "1 and 2 and 7 and 9"
        a.mkString("<", ",", ">")
            // "<1,2,7,9>"

## Arrays Multidimensionales

Para construir un array multidimensional contamos con el medodo _ofDim_ con el cual podemos construir arreglos bidimensionales de forma nativa.

    val matrix = Array.ofDim[Double](3,4)
        // Tres filas, 4 columnas.

Y para acceder a la información,

    matrix(row)(column) = 42

Para hacer que los arreglos varien en el tamaño de las filas tenemos lo siguiente:

    val triangle = new Array[Array[Int]](10)
    for(i <- triangle.indices)
        triangle(i) = new Array[Int](i +1)

Como vemos para arrays que no son matrices usuamos la construcción usual de java.

