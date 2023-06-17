//6. Escribe un loop que computa el producto del valor en 
//Unicode de las letras de un string.
def prodStr(str: String) = {
    var result = 1L
    for(char <- str) result = result * char.toLong
    result
}

//Resuelve el ejercicio anterior sin usar ciclos
def prodStr1(str: String) = {
    str.map(_.toLong).product
}

//Escribe la función anterior de forma recursiva
def prodStr2(args: String) : Long = {
        if (args.tail != "") args.head.toLong * prodStr2(args.tail)
        else args.head.toLong
    }

def pow(x : Int, n:Int) : Double = {
    if (n > 0){
        if (n % 2 == 0) pow(x, n/2) * pow(x, n/2) 
        else x * pow(x, n-1)
    }
    else if (n == 0) 1
    else 1 / pow(x, (-1) * n)
}