package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import scala.util.Random
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.solution.EvaluatedSolution
import akka.actor._
import util.Timeout
/**
 * @author Nemanja
 */
class TSAlgorithm(val maxTabooListSize: Int, val numOfRandomMoves: Int, val neighbourhoodSearch: (List[Int], Int, Int) => List[Int]) extends Algorithm {
  /**
   * A secondary constructor.
   */
  def this(maxTabooListSize: Int, numOfRandomMoves: Int) {
    this(maxTabooListSize, numOfRandomMoves, NeighbourhoodSearch.INSdefineMove)//default values, neighbourhoodSearch:NeighbourhoodSearch.INSdefineMove
  }
  def this(maxTabooListSize: Int) {
    this(maxTabooListSize, 20, NeighbourhoodSearch.INSdefineMove)//default values, maxTabooListSize:7, numOfRandomMoves:20, neighbourhoodSearch:NeighbourhoodSearch.INSdefineMove
  }
  def this() {
    this(7, 20, NeighbourhoodSearch.INSdefineMove)//default values, maxTabooListSize:7, numOfRandomMoves:20, neighbourhoodSearch:NeighbourhoodSearch.INSdefineMove
  }
  def evaluateSmallProblem(p: Problem) = {
    var evBestSolution = initialSolution(p)
    var move = (0, 1) //dummy initialization
    var tabooList: List[Int] = List()
    var allMoves = generateAllNeighbourhoodMoves(p.numOfJobs)
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      var pair = firstImprovement(p, evBestSolution, allMoves, tabooList, expireTimeMillis)
      val neighbourSolution = pair._1
      move = pair._2
      if(neighbourSolution.value < evBestSolution.value)
        evBestSolution = neighbourSolution
      tabooList = updateTabooList(tabooList, neighbourSolution)
    }
    evBestSolution
  }
  
  def evaluateBigProblem(p: Problem) = {
    var evBestSolution = initialSolution(p)
    var move = (0, 1) //dummy initialization
    var tabooList: List[Int] = List()
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    while (Timeout.notTimeout(expireTimeMillis)) {
      //Examine a fixed number of moves that are not taboo, randomly generated. Good method for huge instances
      val allMoves = generateNRandomNeighbourhoodMoves(p.numOfJobs)
      val pair1 = bestImprovement(p, evBestSolution, allMoves, tabooList, expireTimeMillis)
      var evNewSolution = pair1._1
      move = pair1._2
      if(evNewSolution.value < evBestSolution.value)
        evBestSolution = evNewSolution
      tabooList = updateTabooList(tabooList, evNewSolution)
    }
    evBestSolution
  }
  override def evaluate(p: Problem) = {
    if(p.numOfJobs <= 11)
      evaluateSmallProblem(p)
    else
      evaluateBigProblem(p)
  }
  def initialSolution(p: Problem) = {
    val nehAlgorithm = new NEHAlgorithm()
    nehAlgorithm.evaluate(p)
  }
  def updateTabooList(tabooList: List[Int], solution: EvaluatedSolution): List[Int] = {
    if (tabooList.size == maxTabooListSize) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList.drop(1) ::: List(solution.value)
      } else
        tabooList ::: List(solution.value)
  }
  def generateAllNeighbourhoodMoves(numOfJobs: Int): List[(Int, Int)] = {
    //tuples of distinct values, (1,1) is not allowed
    (for (x <- 0 until numOfJobs; y <- 0 until numOfJobs) yield (x, y)).toList.filter(p => p._1 != p._2)
  }
  def isForbidden(tabooList: List[Int], makespan: Int) = {
    tabooList.contains(makespan)//forbidden makespan if it is in taboo list
  }
  def generateNRandomNeighbourhoodMoves(numOfJobs: Int): List[(Int, Int)] = {
    var movesList: List[(Int, Int)] = List()
    var i = 0
    while (i < numOfRandomMoves) {
      val move = NeighbourhoodSearch.randomNeighbourPair(numOfJobs) //firstPoint: [0,numOfJobs-1],secondPoint:  [0, numOfJobs-1], firstPoint!=secondPoint
      movesList = movesList ::: List(move)
      i = i + 1  
    }
    movesList
  }

  //Examine all provided moves and take the first which improves the current solution
  def firstImprovement(p: Problem, evOldSolution: EvaluatedSolution, allMoves: List[(Int, Int)], expireTimeMillis: Double) = {
    var bestSolution = evOldSolution
    var candidateMoves = allMoves
    var move = (0, 1) //dummy initialization
    var betterNotFound = true
    while (betterNotFound && candidateMoves.size != 0 && Timeout.notTimeout(expireTimeMillis)) {
      val perturbed = neighbourhoodSearch(evOldSolution.solution.toList, candidateMoves.head._1, candidateMoves.head._2)
      val newSolution = new Solution(perturbed)
      val evNewSolution = Problem.evaluate(p, newSolution)
      if (evNewSolution.value < bestSolution.value) {
        bestSolution = evNewSolution
        move = candidateMoves.head
        betterNotFound = false
      }
      candidateMoves = candidateMoves.tail
    }
    (bestSolution, move)
  }
  //Examine the moves (that are not taboo) and take the first which improves the current solution
  def firstImprovement(p: Problem, evOldSolution: EvaluatedSolution, allMoves: List[(Int, Int)], tabooList: List[Int], expireTimeMillis: Double) = {
    var bestSolution = evOldSolution
    var candidateMoves = allMoves
    var move = (0, 1) //dummy initialization
    var betterNotFound = true
    while (betterNotFound && candidateMoves.size != 0 && Timeout.notTimeout(expireTimeMillis)) {
      val perturbed = neighbourhoodSearch(evOldSolution.solution.toList, candidateMoves.head._1, candidateMoves.head._2)
      val newSolution = new Solution(perturbed)
      val evNewSolution = Problem.evaluate(p, newSolution)
      if (evNewSolution.value < bestSolution.value && (! isForbidden(tabooList, evNewSolution.value))) {
        bestSolution = evNewSolution
        move = candidateMoves.head
        betterNotFound = false
      }
      candidateMoves = candidateMoves.tail
    }
    (bestSolution, move)
  }
  //Examine all the moves and take the best
  //the neighbourhood must be examined in parallel for big instances
  def bestImprovement(p: Problem, evOldSolution: EvaluatedSolution, allMoves: List[(Int, Int)], expireTimeMillis: Double) = {
    var bestSolution = new EvaluatedSolution(999999999, p.jobs)
    var candidateMoves = allMoves
    var move = (0, 1) //dummy initialization
    while (candidateMoves.size != 0 && Timeout.notTimeout(expireTimeMillis)) {
      val perturbed = neighbourhoodSearch(evOldSolution.solution.toList, candidateMoves.head._1, candidateMoves.head._2)
      val newSolution = new Solution(perturbed)
      val evNewSolution = Problem.evaluate(p, newSolution)
      if (evNewSolution.value < bestSolution.value) {
        bestSolution = evNewSolution
        move = candidateMoves.head
      }
      candidateMoves = candidateMoves.tail
    }
    (bestSolution, move)
  }
  //Examine all the moves (that are not taboo) and take the best
  //the neighbourhood must be examined in parallel for big instances
  def bestImprovement(p: Problem, evOldSolution: EvaluatedSolution, allMoves: List[(Int, Int)], tabooList: List[Int], expireTimeMillis: Double) = {
    var bestSolution = new EvaluatedSolution(999999999, p.jobs)
    var candidateMoves = allMoves
    var move = (0, 1) //dummy initialization
    while (candidateMoves.size != 0 && Timeout.notTimeout(expireTimeMillis)) {
      val perturbed = neighbourhoodSearch(evOldSolution.solution.toList, candidateMoves.head._1, candidateMoves.head._2)
      val newSolution = new Solution(perturbed)
      val evNewSolution = Problem.evaluate(p, newSolution)
      if (evNewSolution.value < bestSolution.value && (! isForbidden(tabooList, evNewSolution.value))) {
        bestSolution = evNewSolution
        move = candidateMoves.head
      }
      candidateMoves = candidateMoves.tail
    }
    (bestSolution, move)
  }
  
}
