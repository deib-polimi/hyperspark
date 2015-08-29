package util

/**
 * @author Nemanja
 */
object ConsolePrinter {
  def print(array: Array[Int]): Unit = {
    println(array.mkString("Array[", ",", "]"))
  }
  def print(matrix: Array[Array[Int]]): Unit = {  
    println("Array[")
    for(i<-0 until matrix.size)
      println(matrix(i).mkString("Array[", ",", "]"))
    println("]")
  }
}