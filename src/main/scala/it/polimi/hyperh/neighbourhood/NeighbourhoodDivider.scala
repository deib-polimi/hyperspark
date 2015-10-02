package it.polimi.hyperh.search

import scala.util.Random
import it.polimi.hyperh.solution.Solution

/**
 * @author Nemanja
 */
object NeighbourhoodDivider {
  
  //DIVISION STRATEGIES
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
  def slidingWindow(solution: Solution, windowSize: Int): Array[Solution] = {
    var array: Array[Solution] = Array()
    val perm = solution.permutation
    for(i <- 0 to perm.size - windowSize){
      val window = perm.drop(i).take(windowSize)
      val allowed = perm.filterNot(window.toSet)
      val arrayTake = Random.shuffle(allowed.toList).toArray
      val leftPart = arrayTake.take(i)
      val rightPart = arrayTake.drop(i)
      val newSol = leftPart ++ window ++ rightPart
      array = array ++ Array(new Solution(newSol))
    }
    array
  }
  
  
  
  
  
}