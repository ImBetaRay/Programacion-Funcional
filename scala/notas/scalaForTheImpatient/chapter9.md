# Archivos y Expresiones Regulares

 ## Leyendo Lineas

 Para leer todas las lineas de un archivo, llamamos al método `getLines` de `scala.io.Source`:

    import scala.io.Source
    val source = Source.fromFile("myfile.txt", "UTF-8")
        //El primer parametdo puede ser un string o un java.io.file
    val lineIterator = source.getLines
    
El resultado es un iterador. Se puede usar para procesar una linea a la vez:

    for(l <- lineIterator) proccess 1

Se puede convertir en un array usando el método `toArray` o `toBuffer`

    val lines = source.getLines.toArray

Tambien se puede leer todo el archivo

    val contents = source.mkString

**Importante: Llama a `close` cuando acabes de usar source.**

## Leyendo Carácteres

Para leer uno a uno los carácteres basta con usar un objeto `source` como iterador pues este hereda de `Iterator[Char]`:

    for (c <- source) procces c

## Leyendo de URLs y otros

Para leer de otras fuentes que no son archivos el objeto `source` ya cuenta con metodos para ello:

    val source1 = Source.fromURL("https://horstmann.com", "UTF-8")
    val source2 = Source.fromString("Hola Mundo!")
    val source3 = Source.stdin //Lee de terminal
    
Tambíen se puede leer de archivos binarios. Sin embargo es neceario usar algunas bibliotecas de Java.
Ejemplo de un archivo a un byte array:

    val file = new File(filename)
    val in = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    in.read(bytes)
    in.close()

## Escribiendo Archivos

Scala no tiene un método predefinido para escribir en archivos. Para escribir se usa `java.io.PrintWriter`, por ejemplo:

    val out = new PrintWriter("number.txt")
    for (i <- 1 to 100) out.println(i)
    out.close()

## Control de Procesos

Scala fue diseñado para escalar de pequeños scripts hasta programas masivos. Es por eso que `scala.sys.process` provee de herramientas para interactuar con la terminal. Así se pueden escribir scripts de terminal con todo el poder que Scala posee. 

Por ejemplo:

    import scala.sys.process._
    "ls -al ..".!

Como resultado `ls -al ..` es ejecutado. 

El paquete `scala.sys.process` contiene una conversión implicita de strings a `ProcessBuilder`. El método `!` ejecuta el proceso.

El resultado de ! es el exit code del programa ejecutado: 0 si se ejecuto exitosamente, o distinto de cero si hubo un fallo.

Si usamos `!!`, el resultado sera un string que contiene el resultado de ejecutar el comando.

Se puede redirigir la salida de un programa a la entrada de otro con `#|`:

    ("ls -al /" #| "grep u").!

Para redirigir la salida a un archivo, se usa `#>`:

    ("ls -al /" #> new File("filelist.txt")).!

Para añadir la salida a un archivo, se udsa `#>>`:

    ("ls -al /etc" #>> new File("filelist.txt")).!

Para tomar la entrada de un archivo usamos `#<`:

    ("grep u" #< new File("filelist.txt")).!

Tambien se puede de URLs:

    ("grep Scala" #< new URL("http://horstmann.com/index.html")).!

