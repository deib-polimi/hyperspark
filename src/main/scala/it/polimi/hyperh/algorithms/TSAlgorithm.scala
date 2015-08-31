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
class TSAlgorithm(val maxTabooListSize: Int, val numOfRandomMoves: Int) extends Algorithm {
  /**
   * A secondary constructor.
   */def this(maxTabooListSize: Int) {
    this(maxTabooListSize, 20)//default values, maxTabooListSize:7, numOfRandomMoves:20
  }
  def this() {
    this(7, 20)//default values, maxTabooListSize:7, numOfRandomMoves:20
  }
  def evaluateSmallProblem(p: Problem) = {
    val permutationList = Random.shuffle(p.jobs.toList)
    val oldSolution = new Solution(permutationList)
    var bestSolution = Problem.evaluate(p, oldSolution)
    var move = (0, 1) //dummy initialization
    var tabooList: List[(Int, Int)] = List()
    var allMoves = generateAllNeighbourhoodMoves(p.numOfJobs)
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      val allowedMoves = filterTabooMoves(allMoves, tabooList)
      var pair = examineN_whole(p, bestSolution, allowedMoves, expireTimeMillis)
      pair = aspiration(p, bestSolution, tabooList, expireTimeMillis)
      val neighbourSolution = pair._1
      move = pair._2
      if(neighbourSolution.value < bestSolution.value)
        bestSolution = neighbourSolution
      if (tabooList.size == maxTabooListSize) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList = tabooList.drop(1) ::: List(move)
      } else
        tabooList = tabooList ::: List(move)
    }
    bestSolution
  }
  def evaluateMediumProblem(p: Problem) = {
    val permutationList = Random.shuffle(p.jobs.toList)
    val oldSolution = new Solution(permutationList)
    var bestSolution = Problem.evaluate(p, oldSolution)
    var move = (0, 1) //dummy initialization
    var tabooList: List[(Int, Int)] = List()
    var allMoves = generateAllNeighbourhoodMoves(p.numOfJobs)
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      val allowedMoves = filterTabooMoves(allMoves, tabooList)
      var pair = examineN_firstBest(p, bestSolution, allowedMoves, expireTimeMillis)
      pair = aspiration(p, bestSolution, tabooList, expireTimeMillis)
      val neighbourSolution = pair._1
      move = pair._2
      if(neighbourSolution.value < bestSolution.value)
        bestSolution = neighbourSolution
      if (tabooList.size == maxTabooListSize) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList = tabooList.drop(1) ::: List(move)
      } else
        tabooList = tabooList ::: List(move)
    }
    bestSolution
  }
  
  def evaluateBigProblem(p: Problem) = {
    val permutationList = Random.shuffle(p.jobs.toList)
    val oldSolution = new Solution(permutationList)
    var bestSolution = Problem.evaluate(p, oldSolution)
    var move = (0, 1) //dummy initialization
    var tabooList: List[(Int, Int)] = List()
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      //Examine a fixed number of moves that are not taboo, randomly generated. Good method for huge instances
      val allowedMoves = generateNRandomNeighbourhoodMoves(p.numOfJobs, numOfRandomMoves, tabooList)
      var pair = examineN_whole(p, bestSolution, allowedMoves, expireTimeMillis)
      pair = aspiration(p, bestSolution, tabooList, expireTimeMillis)
      val neighbourSolution = pair._1
      move = pair._2
      if(neighbourSolution.value < bestSolution.value)
        bestSolution = neighbourSolution
      if (tabooList.size == maxTabooListSize) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList = tabooList.drop(1) ::: List(move)
      } else
        tabooList = tabooList ::: List(move)

    }
    bestSolution
  }
  override def evaluate(p: Problem) = {
    if(p.numOfJobs <= 8)
      evaluateSmallProblem(p)
    else if(p.numOfJobs <= 12)
      evaluateMediumProblem(p)
    else
      evaluateBigProblem(p)
  }
  def generateAllNeighbourhoodMoves(numOfJobs: Int): List[(Int, Int)] = {
    //tuples of distinct values, (1,1) is not allowed
    (for (x <- 0 until numOfJobs; y <- 0 until numOfJobs) yield (x, y)).toList.filter(p => p._1 != p._2)
  }
  def generateNRandomNeighbourhoodMoves(numOfJobs: Int, N: Int, tabooList: List[(Int, Int)]): List[(Int, Int)] = {
    var movesList: List[(Int, Int)] = List()
    var i = 0
    while (i < N) {
      val move = NeighbourhoodSearch.randomNeighbourPair(numOfJobs) //firstPoint: [0,numOfJobs-1],secondPoint:  [0, numOfJobs-1], firstPoint!=secondPoint
      if(! tabooList.contains(move)) {
        movesList = movesList ::: List(move)
        i = i + 1
      }
    }
    movesList
  }
  def filterTabooMoves(allMoves: List[(Int, Int)], tabooList: List[(Int, Int)]): List[(Int, Int)] = {
    allMoves.filterNot(p => tabooList.contains(p))
  }
  //var that accepts a neighbourhood search method from NeighbourhoodSearch functions
  private var neighbourhoodSearch: (List[Int], Int, Int) => List[Int] = NeighbourhoodSearch.INSdefineMove

  def setNeighbourhoodSearch(function: (List[Int], Int, Int) => List[Int]) = {
    neighbourhoodSearch = function
  }
  def applyNeighbourhoodSearch(list: List[Int], firstPoint: Int, secondPoint: Int) = {
    neighbourhoodSearch(list, firstPoint, secondPoint)
  }
  //Examine the moves (that are not taboo) and take the first which improves the current solution
  def examineN_firstBest(p: Problem, evOldSolution: EvaluatedSolution, allowedMoves: List[(Int, Int)], expireTimeMillis: Double) = {
    var bestSolution = evOldSolution
    var candidateMoves = allowedMoves
    var move = (0, 1) //dummy initialization
    var betterNotFound = true
    while (betterNotFound && Timeout.notTimeout(expireTimeMillis)) {
      val perturbed = neighbourhoodSearch(bestSolution.solution.toList, candidateMoves.head._1, candidateMoves.head._2)
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
  
  //Examine all the moves (that are not taboo) and take the best
  //the neighbourhood must be examined in parallel
  def examineN_whole(p: Problem, evOldSolution: EvaluatedSolution, allowedMoves: List[(Int, Int)], expireTimeMillis: Double) = {
    var bestSolution = evOldSolution
    var candidateMoves = allowedMoves
    var move = (0, 1) //dummy initialization
    while (candidateMoves.size != 0 && Timeout.notTimeout(expireTimeMillis)) {
      val perturbed = neighbourhoodSearch(bestSolution.solution.toList, candidateMoves.head._1, candidateMoves.head._2)
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
  //allow a taboo move to be performed if it improves the best solution
  def aspiration(p: Problem, bestSolution: EvaluatedSolution, tabooList: List[(Int, Int)], expireTimeMillis: Double) = {
    examineN_whole(p, bestSolution, tabooList, expireTimeMillis)
  }
}
