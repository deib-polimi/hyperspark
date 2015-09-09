package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Timeout
import scala.util.Random

/**
 * @author Nemanja
 */
abstract class ACOAlgorithm(p: Problem, t0: Double, timeLimit: Double) {
  var T = Array.ofDim[Double](p.numOfJobs, p.numOfJobs)  //desire of setting job i at jth position in solution, i and j have solution.size range
  def trail(i: Int, j: Int): Double = T(i-1)(j-1)
  
  def initializeTrails(t0: Double) = {
    for(i <- 0 until p.numOfJobs)
      for(j <- 0 until p.numOfJobs)
        T(i)(j) = t0
  }
  def initialSolution() = p.evaluatePartialSolution(Random.shuffle(p.jobs.toList))
  
  def initialize() = {
    initializeTrails(t0)
    initialSolution()
  }
  def notStopCondition: Boolean = true
  
  def constructAntSolution(bestSolution: EvaluatedSolution): EvaluatedSolution 
  def localSearch(completeSolution: EvaluatedSolution, expireTimeMillis: Double): EvaluatedSolution
  def updatePheromones(antSolution: EvaluatedSolution, bestSolution: EvaluatedSolution)
  def chosenUpdateSolution(antSolution: EvaluatedSolution, bestSolution: EvaluatedSolution): EvaluatedSolution = bestSolution
  /////////////////////////////////////////////////////
  def evaluate(p: Problem): EvaluatedSolution = {
    var iter = 1
    var bestSolution = initialize()
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    while(notStopCondition && Timeout.notTimeout(expireTimeMillis)) {
      var antSolution = constructAntSolution(bestSolution)//pass global best or ant best solution
      antSolution = localSearch(antSolution, expireTimeMillis)
      if(antSolution.value < bestSolution.value)
        bestSolution = antSolution
      updatePheromones(antSolution, bestSolution)////use global best or ant best solution in impl
      iter = iter + 1
    }
    bestSolution
  }
  def probability(i: Int, j: Int, scheduled: List[Int], notScheduled: List[Int]): Double//to insert job i at position j
}