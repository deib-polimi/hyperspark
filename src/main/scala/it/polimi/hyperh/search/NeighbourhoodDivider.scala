package it.polimi.hyperh.search

import scala.util.Random

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
  
  
  
  
}