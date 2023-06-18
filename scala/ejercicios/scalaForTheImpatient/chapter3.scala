// 1. Write a code snippet that sets a to an array of n random integers
// between 0 (inclusive) and n (exclusive)
def rdmNmbs(n : Int) = {
    val result = new Array[Int](n) 
    val rand = new scala.util.Random
    for (i <- 0 until result.length) result(i) = rand.nextInt(n)
    result
}

// 2. Write a loop that swaps adjacent elements of an array of integers. For example,
//Array(1, 2, 3, 4, 5) becomes Array(2, 1, 4, 3, 5) 
def swapAdjs(arr : Array[Int]) = {
    val result = new Array[Int](arr.length)
    for (i <- 0 until arr.length by 2){
        if (i+1 < arr.length) {
            result(i+1) = arr(i) 
            result(i) = arr(i+1)
        } else {
            result(i) = arr(i)
        }
    } 
    result
}

// Repeat the preceding assignment, but produce a new array with the swapped
// values. Use for /yield .
def swapAdjacent(a: Array[Int]) =
    (for(i <- 0 until a.length) yield
      if (i%2==0 && (i+1)==a.length) a(i)
      else if (i%2==0) a(i+1)
      else a(i-1)
    ).toArray

