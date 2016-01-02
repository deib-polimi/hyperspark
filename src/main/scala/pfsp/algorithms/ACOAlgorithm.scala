package pfsp.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.algorithms.Algorithm
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired
import pfsp.problem.PfsProblem
import pfsp.solution.PfsSolution
import pfsp.solution.BadPfsEvaluatedSolution
import pfsp.solution.PfsEvaluatedSolution

/**
 * @author Nemanja
 */
abstract class ACOAlgorithm(p: PfsProblem, t0: Double, seedOption: Option[PfsSolution]) extends Algorithm {
  def this(p: PfsProblem, t0: Double) {
    this(p, t0, None)
  }
  
  var T = Array.ofDim[Double](p.numOfJobs, p.numOfJobs)  //desire of setting job i at jth position in solution, i and j have solution.size range
  def trail(i: Int, j: Int): Double = T(i-1)(j-1)
  seed = seedOption
  
  def initializeTrails(t0: Double) = {
    for(i <- 0 until p.numOfJobs)
      for(j <- 0 until p.numOfJobs)
        T(i)(j) = t0
  }
  def initialSolution(): PfsEvaluatedSolution = {
    seed match {
      case Some(seed) => seed.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
      case None => p.evaluate(PfsSolution(random.shuffle(p.jobs.toList))).asInstanceOf[PfsEvaluatedSolution]
    }
  }
  
  def initialize() = {
    initializeTrails(t0)
    initialSolution()
  }
  
  def constructAntSolution(bestSolution: PfsEvaluatedSolution): PfsEvaluatedSolution 
  def localSearch(completeSolution: PfsEvaluatedSolution, stopCond: StoppingCondition): PfsEvaluatedSolution
  def updatePheromones(antSolution: PfsEvaluatedSolution, bestSolution: PfsEvaluatedSolution)
  //def chosenUpdateSolution(antSolution: EvaluatedSolution, bestSolution: EvaluatedSolution): EvaluatedSolution = bestSolution
  def probability(i: Int, j: Int, scheduled: List[Int], notScheduled: List[Int]): Double//to insert job i at position j
  /////////////////////////////////////////////////////
  override def evaluate(problem: Problem, stopCond: StoppingCondition): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()
    val bestSol = BadPfsEvaluatedSolution(p)
    
    def loop(bestSol: PfsEvaluatedSolution, iter: Int): PfsEvaluatedSolution = {
      if(stop.isNotSatisfied()) {
        var bestSolution = bestSol
        if(iter == 1) {
          bestSolution = initialize()
        }
        var antSolution = constructAntSolution(bestSolution)//pass global best or ant best solution
        antSolution = localSearch(antSolution, new TimeExpired(300).initialiseLimit())
        if(antSolution.value < bestSolution.value)
          bestSolution = antSolution
        updatePheromones(antSolution, bestSolution)////use global best or ant best solution in impl
        loop(bestSolution, iter + 1)
      }
      else bestSol
    }
    loop(bestSol, 1)
  }
  override def evaluate(problem: Problem) = {
    val p = problem.asInstanceOf[PfsProblem]
    val timeLimit = p.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    evaluate(p, stopCond)
  }
}