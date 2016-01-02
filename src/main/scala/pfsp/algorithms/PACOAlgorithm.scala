package pfsp.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import pfsp.problem.PfsProblem
import pfsp.neighbourhood.NeighbourhoodOperator
import pfsp.solution.PfsSolution
import pfsp.solution.PfsEvaluatedSolution
import it.polimi.hyperh.spark.TimeExpired
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired
import pfsp.solution.BadPfsEvaluatedSolution
import it.polimi.hyperh.spark.StoppingCondition

/**
 * @author Nemanja
 */
class PACOAlgorithm(p: PfsProblem, t0: Double, cand: Int, seedOption: Option[PfsSolution]) 
extends MMMASAlgorithm(p, t0, cand, seedOption) {
  /**
   * A secondary constructor.
   */

  def this(p: PfsProblem, seedOption: Option[PfsSolution]) {
    this(p, 0.2, 5, seedOption)
  }

  def this(p: PfsProblem) {
    this(p, 0.2, 5, None)//default values
  }
  seed = seedOption
  
  def initialSolution(p: PfsProblem): PfsEvaluatedSolution = {
    seed match {
      case Some(seedValue) => seedValue.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
      case None => initNEHSolution(p)
    }
  }
  
  //remove Tmax and Tmin limits
  override def setT(iJob: Int, jPos: Int, newTij: Double) = {
    val i = iJob - 1
    val j = jPos - 1
    T(i)(j) = newTij
  }
  //differential initialization of trails based on the seed solution
  def initializeTrails(bestSolution: PfsEvaluatedSolution) = {
    val seed = bestSolution.permutation
    val Zbest = bestSolution.value
    for(i <- 1 to p.numOfJobs)
      for(j <- 1 to p.numOfJobs) {
        val iJobPos = seed.indexWhere( _ == i) + 1
        val diff = scala.math.abs(iJobPos - j) + 1
        if(diff <= p.numOfJobs/4.0) {
          setT(i, j, (1.0/Zbest))
        } 
        else if(p.numOfJobs/4.0 < diff && diff <= p.numOfJobs/2.0) {
          setT(i, j, (1.0/(2*Zbest)))
        } 
        else {
          setT(i, j, (1.0/(4*Zbest)))
        }
      }
  }
  override def initialSolution() = {
    var solution = initialSolution(p)
    solution = localSearch(solution, new TimeExpired(300).initialiseLimit())
    updateTmax(solution)
    updateTmin
    initializeTrails(solution)
    solution
  }
  override def constructAntSolution(bestSolution: PfsEvaluatedSolution): PfsEvaluatedSolution = {  
    var scheduled: List[Int] = List()
    var jPos = 1
    var notScheduled = (1 to p.numOfJobs).toList//.filterNot(j => scheduled.contains(j))
    var candidates: List[Int] = List()
    
    while(jPos <= p.numOfJobs) {
      var nextJob = -1
      var u = random.nextDouble()
      if(u <= 0.4) {
        nextJob = bestSolution.solution.toList.filterNot(job => scheduled.contains(job)).head
      }
      else if(u <= 0.8) {
        candidates = bestSolution.solution.toList.filterNot(job => scheduled.contains(job)).take(cand)
        var max = 0.0
        while(candidates.size != 0) {
          val sij = sumij(candidates.head, jPos)
          if(sij > max) {
            max = sij
            nextJob = candidates.head
          }
          candidates = candidates.tail
        }//end while
      }
      else {
        candidates = bestSolution.solution.toList.filterNot(job => scheduled.contains(job)).take(cand)
        nextJob = getNextJob(scheduled, candidates, jPos)
      }
      scheduled = scheduled ::: List(nextJob)
      jPos = jPos + 1
    }
    p.evaluatePartialSolution(scheduled)
  }
  override def updatePheromones(antSolution: PfsEvaluatedSolution, bestSolution: PfsEvaluatedSolution) = {
    val usedSolution = antSolution
    val Zcurrent = antSolution.value
    for(i <- 1 to p.numOfJobs)
      for(j <- 1 to p.numOfJobs) {
        val h = antSolution.permutation.indexWhere( _ == i) + 1
        val hbest = bestSolution.permutation.indexWhere( _ == i) + 1
        val x = scala.math.abs(hbest - j) + 1
        val diff = scala.math.pow(x, 1/2.0)
        var newTij: Double = -1.0
        
        if(p.numOfJobs <= 40) {
          if(scala.math.abs(h - j) <= 1){
            newTij = persistenceRate * trail(i,j) + (1.0/(diff*Zcurrent))
          } else {
            newTij = persistenceRate * trail(i,j)
          }
        }
        else {
          if(scala.math.abs(h - j) <= 2){
            newTij = persistenceRate * trail(i,j) + (1.0/(diff*Zcurrent))
          } else {
            newTij = persistenceRate * trail(i,j)
          }
        }
        setT(i, j, newTij)
      }
  }
  //job-index-based swap scheme
  def swapScheme(completeSolution: PfsEvaluatedSolution, stopCond: StoppingCondition): PfsEvaluatedSolution = {
    var bestSolution = completeSolution
    val seed = bestSolution.permutation
    val seedList = seed.toList
    var i = 1
    while(i <= p.numOfJobs && stopCond.isNotSatisfied()){
      var j = 1
      while(j <= p.numOfJobs && stopCond.isNotSatisfied()) {
        if(seed(j-1) != i) {
          val indI = seed.indexWhere( _ == i)
          val indK = j-1
          val neighbourSol = NeighbourhoodOperator(random).SWAPdefineMove(seedList, indI, indK)
          val evNeighbourSol = p.evaluatePartialSolution(neighbourSol)
          if(evNeighbourSol.value < bestSolution.value)
            bestSolution = evNeighbourSol
        }
        j = j + 1
      }//while j
      i = i + 1
    }//while i
    bestSolution
  }
  override def evaluate(problem: Problem, stopCond: StoppingCondition): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val bestSol = BadPfsEvaluatedSolution(p)
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()
    def loop(bestSol: PfsEvaluatedSolution, iter: Int): PfsEvaluatedSolution = {
      if(stop.isNotSatisfied()) {
        var bestSolution = bestSol
        if(iter == 0) {
          bestSolution = initialize()
        }
        var antSolution = constructAntSolution(bestSolution)//pass global best or ant best solution
        antSolution = localSearch(antSolution, new TimeExpired(300).initialiseLimit())
        if(antSolution.value < bestSolution.value)
          bestSolution = antSolution
        updatePheromones(antSolution, bestSolution)////use global best or ant best solution in impl
        if(iter % 40 == 0)
          bestSolution = swapScheme(bestSolution, stop)
        loop(bestSolution, iter + 1)
      }
      else bestSol
    }
    loop(bestSol, 0)
  }
  
  override def evaluate(problem: Problem) = {
    val p = problem.asInstanceOf[PfsProblem]
    val timeLimit = p.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    evaluate(p, stopCond)
  }
  
  override def evaluate(p:Problem, seedSol: Option[Solution], stopCond: StoppingCondition):EvaluatedSolution = {
    seed = seedSol
    evaluate(p, stopCond)
  }
}