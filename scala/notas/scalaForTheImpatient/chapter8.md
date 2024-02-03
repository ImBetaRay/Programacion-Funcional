# Herencia

## Extendiendo una clase

Se hace igual que en Java usando `extend`:

    class Employee extends Person {
        var salary = 0.0
        ...
    }

Igual que en Java se puede pueden especificar atributos y métodos que son nuevos para la subclase o aquellos que metodos que sobreescriben a la clase padre.

Igual que un Java se puede declarar con `final` una clase que no puede ser extendida. Tambien se puede hacer con metodos o atributos para que estos no puedan ser sobreescritos.

## Sobrescribiendo Metodos

En Scala es necesario usar `override` cuando se sobreescribe un método no abstracto. Por ejemplo,

    class Person {
        ...
        override def toString = s"${getClass.getName}[name=$name]"
    }

Para llamar a un metodo de la clase padre se llama al igual que en Java con `super`:

    class Employee extends Person {
        ...
        override def toString = s"${super.toString}[salary = $salary]"
    }

## Construción de la Clase Padre

Como vimos en el capitulo 5 cada clase puede tener un constructor primario y cualquier número de auxiliares. Por lo tanto podemos notar que los constructores auxiliares de una clase hijo deben primero llamar al constructuor primario de la misma y este a su vez llama al de la clase padre. Por ejemplo:

    class Employee(name: String, age: Int, val salary : Double) extends Person(name, age)

Lo que define a la sublase es 

    class Employee(...) extends Person(...)

Y lo que define al constructor primario es

    ... Employee(name: String, age: Int, val salary: Double) ... Person(name, age)

Una clase de Scala puede heredar de una clase de Java. Por ejemplo,

    class PathWriter(p: Path, cs: Charset) extends
        java.io.PrintWriter(Files.newBufferedWriter(p, cs))

## Sobreescribiendo Campos

Se puede sobreescribir un `val` (or un `def` sin parametros) con otro campo con el mismo nombre.

Por ejemplo,

    class Person(val name: String) {
        override def toString = s"${getClass.getName}[name=$name]"
    }

    class SecretAgent(codename: String) extends Person(codename) {
        override val name = "secret" // Don’t want to reveal name . . .
        override val toString = "secret" // . . . or class name
    }

## Abstract Class

Como en Java la palabra reservada `abstract` para denotar que una clase no puede ser instanciada, usualmente porque los metodos no están definidos. Por ejemplo,

    abstract class Person(val name: String) {
        def id: Int
    }

Cuando heredemos de esta clase no es necesario usar `override` basta con que se incluya en la clase.

    class Employee(name: String) extends Person(name) {
        def id = name.hashCode
    }

Adicionalmente una clase absstracta puede tener atributos abstractos que son aquellos que no tienen valor inicial. Por ejemplo,

    abstract class Person {
        val id: Int
        var name: String
    }

Las clases que hereden deben definir estos parametros por ejemplo:

    class Employee(val id : Int) extends Person {
        var name = ""
    }