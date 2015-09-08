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
    var tabooList: List[((Int, Int), (Int, Int))] = List()
    var allMoves = generateAllNeighbourhoodMoves(p.numOfJobs)
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      val allowedMoves = filterTabooMoves(allMoves, tabooList, evBestSolution)
      val pair1 = examineN_whole(p, evBestSolution, allowedMoves, expireTimeMillis)
      val pair2 = aspiration(p, evBestSolution, tabooList, expireTimeMillis)
      var evNewSolution = pair1._1
      move = pair1._2
      if(pair2._1.value < pair1._1.value) {
        evNewSolution = pair2._1
        move = pair2._2
      }
      if(evNewSolution.value < evBestSolution.value)
        evBestSolution = evNewSolution
      tabooList = updateTabooList(tabooList, move, evBestSolution)
    }
    evBestSolution
  }
  def evaluateMediumProblem(p: Problem) = {
    var evBestSolution = initialSolution(p)
    var move = (0, 1) //dummy initialization
    var tabooList: List[((Int, Int), (Int, Int))] = List()
    var allMoves = generateAllNeighbourhoodMoves(p.numOfJobs)
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      val allowedMoves = filterTabooMoves(allMoves, tabooList, evBestSolution)
      var pair = examineN_firstBest(p, evBestSolution, allowedMoves, expireTimeMillis)
      pair = aspiration(p, evBestSolution, tabooList, expireTimeMillis)
      val neighbourSolution = pair._1
      move = pair._2
      if(neighbourSolution.value < evBestSolution.value)
        evBestSolution = neighbourSolution
      tabooList = updateTabooList(tabooList, move, evBestSolution)
    }
    evBestSolution
  }
  
  def evaluateBigProblem(p: Problem) = {
    var evBestSolution = initialSolution(p)
    var move = (0, 1) //dummy initialization
    var tabooList: List[((Int, Int), (Int, Int))] = List()
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      //Examine a fixed number of moves that are not taboo, randomly generated. Good method for huge instances
      val allowedMoves = generateNRandomNeighbourhoodMoves(p.numOfJobs, tabooList, evBestSolution)
      val pair1 = examineN_whole(p, evBestSolution, allowedMoves, expireTimeMillis)
      val pair2 = aspiration(p, evBestSolution, tabooList, expireTimeMillis)
      var evNewSolution = pair1._1
      move = pair1._2
      if(pair2._1.value < pair1._1.value) {
        evNewSolution = pair2._1
        move = pair2._2
      }
      if(evNewSolution.value < evBestSolution.value)
        evBestSolution = evNewSolution
      tabooList = updateTabooList(tabooList, move, evBestSolution)

    }
    evBestSolution
  }
  override def evaluate(p: Problem) = {
    if(p.numOfJobs <= 8)
      evaluateSmallProblem(p)
    else if(p.numOfJobs <= 12)
      evaluateMediumProblem(p)
    else
      evaluateBigProblem(p)
  }
  def initialSolution(p: Problem) = {
    val nehAlgorithm = new NEHAlgorithm()
    nehAlgorithm.evaluate(p)
  }
  def isForbidden(tabooList: List[((Int, Int),(Int, Int))], evOldSolution: EvaluatedSolution, move: (Int,Int)): Boolean = {
    var answer = false
    val P = evOldSolution.solution
    val a = move._1
    val b = move._2
    val forbiddenJobPairs = tabooList.map(t => t._2)
    if(a < b) {
      for(j <- (a+1) to b) {
        val pair = (P(j), P(a))
        if(forbiddenJobPairs.contains(pair))
          answer = true
      }
    }
    else {  //b <= a
      for(j <- b to (a-1)) {
        val pair = (P(a), P(j))
        if(forbiddenJobPairs.contains(pair))
          answer = true
      }
    }
    answer
  }
  
  def filterTabooMoves(allMoves: List[(Int, Int)], tabooList: List[((Int, Int), (Int, Int))], evOldSolution: EvaluatedSolution): List[(Int, Int)] = {
    allMoves.filterNot(move => isForbidden(tabooList, evOldSolution, move))
  }
  def generateAllNeighbourhoodMoves(numOfJobs: Int): List[(Int, Int)] = {
    //tuples of distinct values, (1,1) is not allowed
    (for (x <- 0 until numOfJobs; y <- 0 until numOfJobs) yield (x, y)).toList.filter(p => p._1 != p._2)
  }
  def generateNRandomNeighbourhoodMoves(numOfJobs: Int, tabooList: List[((Int,Int), (Int,Int))], evOldSolution: EvaluatedSolution): List[(Int, Int)] = {
    var movesList: List[(Int, Int)] = List()
    var i = 0
    while (i < numOfRandomMoves) {
      val move = NeighbourhoodSearch.randomNeighbourPair(numOfJobs) //firstPoint: [0,numOfJobs-1],secondPoint:  [0, numOfJobs-1], firstPoint!=secondPoint
      if(! isForbidden(tabooList, evOldSolution, move)) {
        movesList = movesList ::: List(move)
        i = i + 1
      }
    }
    movesList
  }

  //Examine the moves (that are not taboo) and take the first which improves the current solution
  def examineN_firstBest(p: Problem, evOldSolution: EvaluatedSolution, allowedMoves: List[(Int, Int)], expireTimeMillis: Double) = {
    var bestSolution = evOldSolution
    var candidateMoves = allowedMoves
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
  
  //Examine all the moves (that are not taboo) and take the best
  //the neighbourhood must be examined in parallel
  def examineN_whole(p: Problem, evOldSolution: EvaluatedSolution, allowedMoves: List[(Int, Int)], expireTimeMillis: Double) = {
    var bestSolution = new EvaluatedSolution(999999999, p.jobs)
    var candidateMoves = allowedMoves
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
  //allow a taboo move to be performed if it improves the best solution
  def aspiration(p: Problem, bestSolution: EvaluatedSolution, tabooList: List[((Int,Int), (Int,Int))], expireTimeMillis: Double) = {
    val tabooMoves = tabooList.map(t => t._1).map(p => (p._2,p._1))
    examineN_whole(p, bestSolution, tabooMoves, expireTimeMillis)
  }
  def updateTabooList(tabooList: List[((Int, Int), (Int, Int))], move: (Int, Int), evOldSolution: EvaluatedSolution): List[((Int, Int), (Int, Int))] = {
    val a = move._1
    val b = move._2
    val P = evOldSolution.solution
    var t = ((0,0), (1,1))  //(move, jobs), dummy init
    if(a < b)
      t = ((a,b), (P(a), P(a+1)))
    else //b <= a
      t = ((a,b), (P(a-1), P(a)))
    if (tabooList.size == maxTabooListSize) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList.drop(1) ::: List(t)
      } else
        tabooList ::: List(t)
  }
}
