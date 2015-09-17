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
class TSAlgorithm(
    val maxTabooListSize: Int, 
    val numOfRandomMoves: Int, 
    val neighbourhoodSearch: (List[Int], Int, Int) => List[Int],
    sd: Option[Solution]
    ) extends Algorithm {
  /**
   * A secondary constructor.
   */
  def this(maxTabooListSize: Int, numOfRandomMoves: Int) {
    this(maxTabooListSize, numOfRandomMoves, NeighbourhoodSearch.INSdefineMove, None)//default values, neighbourhoodSearch:NeighbourhoodSearch.INSdefineMove
  }
  def this(maxTabooListSize: Int) {
    this(maxTabooListSize, 20, NeighbourhoodSearch.INSdefineMove, None)//default values, maxTabooListSize:7, numOfRandomMoves:20, neighbourhoodSearch:NeighbourhoodSearch.INSdefineMove
  }
  def this() {
    this(7, 20, NeighbourhoodSearch.INSdefineMove, None)//default values, maxTabooListSize:7, numOfRandomMoves:20, neighbourhoodSearch:NeighbourhoodSearch.INSdefineMove
  }
  def this(seed: Option[Solution]) {
    this(7, 20, NeighbourhoodSearch.INSdefineMove, seed)
  }
  private var seed = sd
  
  def initNEHSolution(p: Problem) = {
    val nehAlgorithm = new NEHAlgorithm()
    nehAlgorithm.evaluate(p)
  }
  def initialSolution(p: Problem): EvaluatedSolution = {
    seed match {
      case Some(seed) => Problem.evaluate(p, seed)
      case None => initNEHSolution(p)
    }
  }
  //with default time limit
  def evaluateSmallProblem(p: Problem): EvaluatedSolution = {
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    evaluateSmallProblem(p, timeLimit)
  }
  def evaluateSmallProblem(p: Problem, timeLimit: Double): EvaluatedSolution = {
    var evBestSolution = new EvaluatedSolution(999999999, p.jobs)//dummy initalization
    var allMoves: List[(Int,Int)] = List()//dummy initalization
    //algorithm time limit
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    def loop(bestSolution: EvaluatedSolution, taboo: List[Int], iter: Int): EvaluatedSolution = {
      if(Timeout.notTimeout(expireTimeMillis)) {
        if(iter == 1) {
          evBestSolution = initialSolution(p)
          allMoves = NeighbourhoodSearch.generateAllNeighbourhoodMoves(p.numOfJobs)
        } else {
          evBestSolution = bestSolution
        }
        val pair = firstImprovement(p, evBestSolution, allMoves, taboo, expireTimeMillis)
        val neighbourSolution = pair._1
        if(neighbourSolution.value < evBestSolution.value)
          evBestSolution = neighbourSolution
        val tabooList = updateTabooList(taboo, neighbourSolution)
        loop(evBestSolution, tabooList, iter + 1)
      }
      evBestSolution
    }
    loop(evBestSolution, List(), 1)
  }
  //with default time limit
  def evaluateBigProblem(p: Problem): EvaluatedSolution = {
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    evaluateBigProblem(p, timeLimit)
  }
  def evaluateBigProblem(p: Problem, timeLimit: Double): EvaluatedSolution = {
    var evBestSolution = new EvaluatedSolution(999999999, p.jobs)//dummy initalization
    //algorithm time limit
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    def loop(bestSolution: EvaluatedSolution, taboo: List[Int], iter: Int): EvaluatedSolution = {
      if(Timeout.notTimeout(expireTimeMillis)) {
        if(iter == 1) {
          evBestSolution = initialSolution(p)
        } else {
          evBestSolution = bestSolution
        }
        //Examine a fixed number of moves that are not taboo, randomly generated. Good method for huge instances
        val allMoves = NeighbourhoodSearch.generateNRandomNeighbourhoodMoves(p.numOfJobs, numOfRandomMoves)
        val pair1 = bestImprovement(p, evBestSolution, allMoves, taboo, expireTimeMillis)
        val evNewSolution = pair1._1
        if(evNewSolution.value < evBestSolution.value)
          evBestSolution = evNewSolution
        val tabooList = updateTabooList(taboo, evNewSolution)
        loop(evBestSolution, tabooList, iter + 1)
      }
      else evBestSolution
    }
    loop(evBestSolution, List(), 1)
  }
  override def evaluate(p: Problem) = {
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    if(p.numOfJobs <= 11)
      evaluateSmallProblem(p)
    else
      evaluateBigProblem(p)
  }
  override def evaluate(p:Problem, timeLimit: Double): EvaluatedSolution = {
    if(p.numOfJobs <= 11)
      evaluateSmallProblem(p, timeLimit)
    else
      evaluateBigProblem(p, timeLimit)
  }
  override def evaluate(p:Problem, seedSol: Option[Solution], timeLimit: Double):EvaluatedSolution = {
    seed = seedSol
    evaluate(p, timeLimit)
  }
  def updateTabooList(tabooList: List[Int], solution: EvaluatedSolution): List[Int] = {
    if (tabooList.size == maxTabooListSize) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList.drop(1) ::: List(solution.value)
      } else
        tabooList ::: List(solution.value)
  }
  
  def isForbidden(tabooList: List[Int], makespan: Int) = {
    tabooList.contains(makespan)//forbidden makespan if it is in taboo list
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
