# CLASES    

## Clases Simples y Metodos sin Parametros

En su forma más simple las clases de Scala son muy parecidas a las de Java:

    class Counter {
        private var value = 0 //Se debe inicializar el campo.
        def increment() : Unit = value += 1 //Los metodos son publicos por default.
        def current() = value
    }

En Scala, una clase no es declarada como publica. Un archivo de Scala puede contener multiples clases, con todas ellas con visibilidad publica.

Para usar una clase, se construyen objectos y se llaman metodos de la forma usual en Java.

    val myCounter = new Counter //Tambien se puede new Counter()
    myCounter.increment()
    println(myCounter.current())

## Propiedades con Getters y Setters

Cuando escribimos clases en otros lenguajes orientados a objectos, no solemos usar atributos publicos pues no es agradable que cualquiera pueda acceder de forma directa a nuestro atributo. Es por eso que se prefieren los getters/setters.

Scala nativamente provee un getter y un setter para cada campo. Por ejemplo:

    class Person {
        var age = 0
    }

Scala genera una clase para la JVM con un campo privado _age_ y con los respectivos getters y setters. Estos metodos son publicos pues no declaramos _age_ como _private_.

En Scala, los metodos getter y setter son llamados _age_ y _age_=_ . Por ejemplo,

    println(fred.age) // Llama al metodo fred.age()
    fred.age = 21 // Llama al metodo fred.age_=(21)

En cualquier momento se pueden redefinir los getters y setters según lo requiera el programador. Por ejemplo, 

    class Person {
        private var privateAge = 0 // La volvemos privada y renombramos

        def age = privateAge
        def age_= (newValue: Int) : Unit = {
            if (newValue > privateAge) privateAge = newValue;
        }
    }

_Volvemos la variable privada para que se generen de forma privada los getters y setters. Si la variable es val unicamente se genera un getter._

En Scala un metodo en la clase puede acceder a todos los campos privados de todos los objetos de la misma. Por ejemplo,

    class Counter {
        private var value = 0
        def increment () : Unit = { value += 1 }
        
        def isLess(other : Counter) = value < other.value
            // Puedes acceder a value de todos los elementos.
    }

Scala permite un grado más fuerte de privacidad **private[this]**. En este grado de privacidad como su nombre lo indica cada metodo de la clase solo puede acceder a la instancia a la que pertenece.


## Constructores Auxiliares

En Scala cada clase puede tener tantos contructores como se desea, sin embargo hay uno que es más importante que todos los otros, se llama _primary constructor_. En adición a este cada clase tiene cualquier número de constructores auxiliares.

 1. Los constructores Auxiliares son llamados _this_.
 2. Cada constructor auxiliar debe empezar con una llamada a un metodo auxiliar anteriormente definido o al constructor primario.

Por ejemplo:

    class Person {
        private var name = ""
        private var age = 0

        def this(name: String) {
            this()
            this.name = name
        }

        def this(name: String, age: Int){
            this(name)
            this.age = age
        }
    }

## Constructor Primario

En Scala cada clase tiene un constructor primario. Este constructor no usa el metodo _this_. En su lugar se deduce de la definición de la clase.

1. Los parametros del constructor primario estan puestas inmediatamente despues del nombre de la clase.

        class Person(val name: String, val age: Int) {
            // Parametros
            ...
        }

