# Objetos

## Singletons

En Scala se puede definir una sola instancia de la clase con todas las caracteristicas que se desean. Por ejemplo,

    object Accounts {
        private var lastNumber = 0
        def newUniqueNumber() = { lastNumber += 1; lastNumber}
    }

En Scala el constructor de un singleton será ejecutado hasta que este objeto sea usado, en nuestro ejemplo el constructor se ejecutará hasta que llamemos por primera vez a `Accounts.newUniqueNumber()`. 

## Companion Ojects

En Java una clase puede tener metodos y metodos staticos, para replicar esta estructura en Scala es necesario darle a la clase un *"companion object"* con el mismo nombre. Por ejemplo,

    class Account {
        val id = Account.newUniqueNumber()
        private var balance = 0.0
        def deposit(amount: Double) = {balance += amount}
    }
    object Account {
        private var lastNumber = 0
        def newUniqueNumber() = {lastNumber +=1; lastNumber}
    }


Es necesario que el objeto y la clase esten en el mismo archivo.

## Objetos que Extienden una Clase

Al igual que en java cuando se extiende una clase se tiene todas las propiedades de la clase padre y las adicionales que sean necesaria. Por ejemplo resulta útil si queremos una clase para undoable actions.

    abstract class UndoableAction(val description: String) {
        def undo(): Unit
        def redo(): Unit
    }

    object DoNothingAction extends UndoableAction("Do nothing"){
        override def undo() {}
        override def redo() {}
    }

## El metodo `apply`

Usualmente los objetos tienen un metodo `apply`.  Este metodo de llama de la forma

    Object(arg1, ..., argN)

Usualmente el metodo apply te devuelve un objeto de la clase acompañante. Por ejemplo para la clase Array tenemos 

    Array("Dulce", "Soledad")

