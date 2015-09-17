package it.polimi.hyperh.worksheets
import scala.util.Random

object testFramework {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(128); 
  println("Welcome to the Scala worksheet");$skip(500); 
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
  };System.out.println("""slidingWindow: (solution: List[Int], windowSize: Int)List[List[Int]]""");$skip(526); 
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
  };System.out.println("""slidingWindow: (solution: Array[Int], windowSize: Int)Array[Array[Int]]""");$skip(47); val res$0 = 
  slidingWindow(List(1,2,3,4,5,6,7,8,9,10), 3);System.out.println("""res0: List[List[Int]] = """ + $show(res$0));$skip(48); val res$1 = 
  slidingWindow(Array(1,2,3,4,5,6,7,8,9,10), 3);System.out.println("""res1: Array[Array[Int]] = """ + $show(res$1))}
}
