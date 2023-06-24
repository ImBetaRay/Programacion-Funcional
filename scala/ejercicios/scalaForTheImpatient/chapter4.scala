// Write a program that reads words from a file. Use a mutable map to count
// how often each word appears. 
def countWords(path : String) = {
    val in = new java.util.Scanner(new java.io.File(path))
    val result = scala.collection.mutable.Map[String, Int]()
    while (in.hasNext()){
        val aux = in.next()
        if(!result.contains(aux)) result(aux) = 0 else result(aux) += 1
        result 
    }
}

