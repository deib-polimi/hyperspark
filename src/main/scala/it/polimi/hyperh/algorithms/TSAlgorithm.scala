package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import scala.util.Random
import it.polimi.hyperh.search.NeighbourhoodSearch



/**
 * @author Nemanja
 */
object TSAlgorithm {
  def filterMoves(allowedMoves: List[(Int,Int)], tabooList: List[(Int,Int)]): List[(Int, Int)] = {
    allowedMoves.filterNot(p => tabooList.contains(p))
  }
  def generateDistinctPairs(numOfJobs: Int): List[(Int,Int)] = {
    //tuples of distinct values, (1,1) is not allowed
    (for(x <- 0 until numOfJobs; y <- 0 until numOfJobs) yield (x, y)).toList.filter(p => p._1 != p._2)
  }
  def evaluate(p: Problem, maxTabooListSize: Int) = {
    val permutationList = Random.shuffle(p.jobs.toList)
    val oldSolution = new Solution(permutationList)
    var evOldSolution = Problem.evaluate(p, oldSolution)
    var bestSolution = evOldSolution
    var move = (1,2)  //dummy initialization
    var tabooList: List[(Int,Int)] = List()
    var allMoves = generateDistinctPairs(p.numOfJobs)
    def notStopCondition() = ???
    while(notStopCondition()) {
      val allowedMoves = filterMoves(allMoves, tabooList).toArray
      for(i <- 0 until allowedMoves.size) {
        val perturbed = NeighbourhoodSearch.INSdefineMove(evOldSolution.solution.toList, allowedMoves(i)._1, allowedMoves(i)._2)
        val newSolution = new Solution(perturbed)
        val evNewSolution = Problem.evaluate(p, newSolution)
        if(evNewSolution.value < bestSolution.value) {
          bestSolution = evNewSolution
          move = allowedMoves(i)
        }
        if(tabooList.size == maxTabooListSize){
          //remove the oldest forbidden move, and add new move at the end
          tabooList = tabooList.drop(1) ::: List(move)
        }
      }
      //val pair = NeighbourhoodSearch.INSreturnMove(old)
     
    }
  }
}