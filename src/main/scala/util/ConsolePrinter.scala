package util

/**
 * @author Nemanja
 */
object ConsolePrinter {
  def print[T](array: Array[T]): Unit = {
    println(array.mkString("Array[", ",", "]"))
  }
  def print[T](matrix: Array[Array[T]]): Unit = {  
    println("Array[")
    for(i<-0 until matrix.size)
      println(matrix(i).mkString("Array[", ",", "]"))
    println("]")
  }
}