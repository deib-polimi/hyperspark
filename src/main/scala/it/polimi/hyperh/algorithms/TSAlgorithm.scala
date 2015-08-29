package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import scala.util.Random
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.solution.EvaluatedSolution
import akka.actor._
/**
 * @author Nemanja
 */
object TSAlgorithm {
  def generateAllNeighbourhoodMoves(numOfJobs: Int): List[(Int, Int)] = {
    //tuples of distinct values, (1,1) is not allowed
    (for (x <- 0 until numOfJobs; y <- 0 until numOfJobs) yield (x, y)).toList.filter(p => p._1 != p._2)
  }
  def generateNRandomNeighbourhoodMoves(numOfJobs: Int, N: Int): List[(Int, Int)] = {
    var movesList: List[(Int, Int)] = List()
    while (movesList.size < N) {
      val move = NeighbourhoodSearch.randomNeighbourPair(N) //firstPoint: [0,n-1],secondPoint:  [0, n-1], firstPoint!=secondPoint
      movesList ::: List(move)
    }
    movesList
  }
  def filterTabooMoves(allowedMoves: List[(Int, Int)], tabooList: List[(Int, Int)]): List[(Int, Int)] = {
    allowedMoves.filterNot(p => tabooList.contains(p))
  }
  private var neighbourhoodSearchMethod: (List[Int], Int, Int) => List[Int] = NeighbourhoodSearch.INSdefineMove

  def setNeighbourhoodSearch(function: (List[Int], Int, Int) => List[Int]) = {
    neighbourhoodSearchMethod = function
  }
  def applyNeighbourhoodSearch(list: List[Int], firstPoint: Int, secondPoint: Int) = {
    neighbourhoodSearchMethod(list, firstPoint, secondPoint)
  }
  //Examine the moves (that are not taboo) and take the first which improves the current solution
  def getFirstBestSolution(p: Problem, evOldSolution: EvaluatedSolution, allowedMoves: List[(Int, Int)]) = {
    var i = 0
    var bestSolution = evOldSolution
    var move = (0, 1) //dummy initialization
    var betterNotFound = true
    while (i < allowedMoves.size && betterNotFound) {
      val perturbed = applyNeighbourhoodSearch(evOldSolution.solution.toList, allowedMoves(i)._1, allowedMoves(i)._2)
      val newSolution = new Solution(perturbed)
      val evNewSolution = Problem.evaluate(p, newSolution)
      if (evNewSolution.value < bestSolution.value) {
        bestSolution = evNewSolution
        move = allowedMoves(i)
        betterNotFound = false
      }
      i = i + 1
    }
    (bestSolution, move)
  }
  //Examine a fixed number of moves that are not taboo, randomly generated. Good method for huge instances
  //combination of next method and generation of random moves for allowed moves
  //var fixedNumOfMoves = 100

  //Examine all the moves (that are not taboo) and take the best
  //the neighbourhood must be examined in parallel
  def getBestSolution(p: Problem, evOldSolution: EvaluatedSolution, allowedMoves: List[(Int, Int)]) = {
    var i = 0
    var bestSolution = evOldSolution
    var move = (0, 1) //dummy initialization
    while (i < allowedMoves.size) {
      val perturbed = applyNeighbourhoodSearch(evOldSolution.solution.toList, allowedMoves(i)._1, allowedMoves(i)._2)
      val newSolution = new Solution(perturbed)
      val evNewSolution = Problem.evaluate(p, newSolution)
      if (evNewSolution.value < bestSolution.value) {
        bestSolution = evNewSolution
        move = allowedMoves(i)
      }
      i = i + 1
    }
    (bestSolution, move)
  }

  def evaluate(p: Problem, maxTabooListSize: Int) = {
    val permutationList = Random.shuffle(p.jobs.toList)
    val oldSolution = new Solution(permutationList)
    var evOldSolution = Problem.evaluate(p, oldSolution)
    var bestSolution = evOldSolution
    var move = (0, 1) //dummy initialization
    var tabooList: List[(Int, Int)] = List()
    var allMoves = generateAllNeighbourhoodMoves(p.numOfJobs)
    def notStopCondition() = true
    while (notStopCondition()) {
      val allowedMoves = filterTabooMoves(allMoves, tabooList)
      val pair = getBestSolution(p, evOldSolution, allowedMoves)
      bestSolution = pair._1
      move = pair._2
      if (tabooList.size == maxTabooListSize) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList = tabooList.drop(1) ::: List(move)
      } else
        tabooList = tabooList ::: List(move)
      //val pair = NeighbourhoodSearch.INSreturnMove(old)

    }
  }
}