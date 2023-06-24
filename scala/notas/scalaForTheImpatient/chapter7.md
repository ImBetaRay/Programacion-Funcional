# Packages e Imports

## Packages

Los paquetes en Scala cumplen el mismo proposito que en Java: para manejar nombres en un pograma muy grande. Por ejemplo, el nombre _Map_ puede aparecer en el paquete `scala.collection.inmutable` y `scala.collection.mutable` sin generar conflictos. Para acceder a cualquiera especificamente, se puede usar el nombre completo `scala.collection.mutable.Map`.

Para añadir elementos a un paquete, se incluyen en las ordenes de paquetes, por ejemplo:

    package com {
        package horstmann {
            package impatient {
                class Employee
                ...
            }
        }
    }

Entonces la clase `Employee` puede ser accedida desde donde sea como `com.horstmann.impatient.Employee`.

A diferencia de una clase o un objeto, un paquete se puede definir a traves de multiples archivos. La clase anterior puede estár en el archivo `Employee.scala` y el archivo `Manager.scala` puede contener

    package com {
        package horstmann {
            package impatient {
                class Manager
                ...
            }
        }
    }

De igual forma, se puede contribuir a más de un paquete en un solo archivo. Por ejemplo el `Employee.scala` puede contener

    package com {
        package horstmann {
            package impatient {
                class Employee
                ...
            }
        }
    }
    package net {
        package bigjava {
            class Counter
            ...
        }
    }

## Reglas de Alcance

En Scala, las reglas de alcance son más consistentes que en Java. Los paquetes anidados en Scala se comportan como cualquier otro bloque, es decir puedes acceder a nombres del bloque exterior. Por ejemplo

    package com {
        package horstmann {
            object Utils {
                def percentOf(value: Double, rate: Double) = value + rate / 100
                ...
            }

            package impatient {
                class Employee {
                    ...
                    def giveRaise(rate: Scala.Double) = 
                        salary += Utils.percentOf(salary,rate)
                }
            }
        }
    }

## Notación al Principio

En lugar de la notación anidada, se puede usar una clausula al principio del archivo sin necesidad de llaves. Por ejemplo:

    package com.horstmann.impatient
    package people

    class Person
    ...

Es equivalente a 

    package com.horstmann.impatient {
        package people {
            class Person
            ...
            //Hasta el final del archivo.
        }
    }

Esta notación al inicio es la más usual si todo el archivo pertenece al mismo paquete.

## Package Objects

Un paquete puede contener clases, objetos y traits, pero no la definición de funciones o variable. Es una limitación de JVM. Packages objects lidian con este problema.

Cada paquete puede tener un **package object**. Se define en el paquete padre y tiene el nombre del paquete hijo, por ejemplo,

    package com.horstmann.impatient
    
    package object people {
        val defaultName = "John Q. Public"
    }

    package people {
        class Person {
            var name = defaultName 
        }
        ...
    }

Notemos que `defaultName` no necesita más para ser accedido pues está en el mismo paquete. En cualquier otro lugar es accesible como  `com.horstmann.impatient.people.defaultName`.