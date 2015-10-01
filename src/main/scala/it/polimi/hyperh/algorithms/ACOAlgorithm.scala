package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Timeout
import scala.util.Random
import it.polimi.hyperh.solution.Solution
import util.RNG
import it.polimi.hyperh.solution.DummyEvaluatedSolution

/**
 * @author Nemanja
 */
abstract class ACOAlgorithm(p: Problem, t0: Double, seedOption: Option[Solution]) extends Algorithm {
  def this(p: Problem, t0: Double) {
    this(p, t0, None)
  }
  
  var T = Array.ofDim[Double](p.numOfJobs, p.numOfJobs)  //desire of setting job i at jth position in solution, i and j have solution.size range
  def trail(i: Int, j: Int): Double = T(i-1)(j-1)
  seed = seedOption
  def notStopCondition: Boolean = true
  
  def initializeTrails(t0: Double) = {
    for(i <- 0 until p.numOfJobs)
      for(j <- 0 until p.numOfJobs)
        T(i)(j) = t0
  }
  def initialSolution(): EvaluatedSolution = {
    seed match {
      case Some(seed) => seed.evaluate(p)
      case None => p.evaluate(Solution(random.shuffle(p.jobs.toList)))
    }
  }
  
  def initialize() = {
    initializeTrails(t0)
    initialSolution()
  }
  
  def constructAntSolution(bestSolution: EvaluatedSolution): EvaluatedSolution 
  def localSearch(completeSolution: EvaluatedSolution, expireTimeMillis: Double): EvaluatedSolution
  def updatePheromones(antSolution: EvaluatedSolution, bestSolution: EvaluatedSolution)
  //def chosenUpdateSolution(antSolution: EvaluatedSolution, bestSolution: EvaluatedSolution): EvaluatedSolution = bestSolution
  def probability(i: Int, j: Int, scheduled: List[Int], notScheduled: List[Int]): Double//to insert job i at position j
  /////////////////////////////////////////////////////
  override def evaluate(p: Problem, timeLimit: Double): EvaluatedSolution = {
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    val bestSol = DummyEvaluatedSolution(p)
    
    def loop(bestSol: EvaluatedSolution, iter: Int): EvaluatedSolution = {
      if(notStopCondition && Timeout.notTimeout(expireTimeMillis)) {
        var bestSolution = bestSol
        if(iter == 1) {
          bestSolution = initialize()
        }
        var antSolution = constructAntSolution(bestSolution)//pass global best or ant best solution
        antSolution = localSearch(antSolution, Timeout.setTimeout(300))
        if(antSolution.value < bestSolution.value)
          bestSolution = antSolution
        updatePheromones(antSolution, bestSolution)////use global best or ant best solution in impl
        loop(bestSolution, iter + 1)
      }
      else bestSol
    }
    loop(bestSol, 1)
  }
  override def evaluate(p: Problem) = {
    val timeLimit = p.numOfMachines*(p.numOfJobs/2.0)*60//termination is n*(m/2)*60 milliseconds
    evaluate(p, timeLimit)
  }
}