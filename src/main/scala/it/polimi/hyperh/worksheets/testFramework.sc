package it.polimi.hyperh.worksheets
import scala.util.Random

object testFramework {
  println("Welcome to the Scala worksheet")
  def slidingWindow(solution: List[Int], windowSize: Int): List[List[Int]] = {
    var list: List[List[Int]] = List()
    for(i <- 0 to solution.size - windowSize){
      val window = solution.drop(i).take(windowSize)
      val allowed = solution.filterNot(window.toSet)
      val listTake = Random.shuffle(allowed)
      val leftPart = listTake.take(i)
      val rightPart = listTake.drop(i)
      val newSol = leftPart ::: window ::: rightPart
      list = list ::: List(newSol)
    }
    list
  }
  def slidingWindow(solution: Array[Int], windowSize: Int): Array[Array[Int]] = {
    var array: Array[Array[Int]] = Array()
    for(i <- 0 to solution.size - windowSize){
      val window = solution.drop(i).take(windowSize)
      val allowed = solution.filterNot(window.toSet)
      val arrayTake = Random.shuffle(allowed.toList).toArray
      val leftPart = arrayTake.take(i)
      val rightPart = arrayTake.drop(i)
      val newSol = leftPart ++ window ++ rightPart
      array = array ++ Array(newSol)
    }
    array
  }
  slidingWindow(List(1,2,3,4,5,6,7,8,9,10), 3)
  slidingWindow(Array(1,2,3,4,5,6,7,8,9,10), 3)
}