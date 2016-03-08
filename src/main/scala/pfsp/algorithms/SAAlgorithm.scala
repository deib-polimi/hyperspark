package pfsp.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.Algorithm
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired
import pfsp.neighbourhood.NeighbourhoodOperator
import pfsp.problem.PfsProblem
import pfsp.solution.PfsEvaluatedSolution
import pfsp.solution.PfsSolution
import pfsp.solution.NaivePfsEvaluatedSolution

/**
 * @author Nemanja
 */
class SAAlgorithm(p: PfsProblem) extends Algorithm {

  var temperatureUB: Double = p.sumJobTimesMatrix() / (5.0*p.numOfJobs*p.numOfMachines)
  var temperatureLB: Double = 1.0
  var iterations: Double = scala.math.max(3300*scala.math.log(p.numOfJobs)+7500*scala.math.log(p.numOfMachines)-18250, 2000)
  var coolingRate: Double = (temperatureUB-temperatureLB)/((iterations-1)*temperatureUB*temperatureLB)
  /**
   * A secondary constructor.
   */
  def this(p: PfsProblem, tUB: Double, tLB: Double, cRate: Double) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
  }
  def this(p: PfsProblem, tUB: Double, tLB: Double, cRate: Double, seedOption: Option[PfsSolution]) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
    seed = seedOption
  }

  def this(p: PfsProblem, seedOption: Option[PfsSolution]) {
    this(p)
    seed = seedOption
  }

  def initialSolution(p: PfsProblem): PfsEvaluatedSolution = {
    seed match {
      case Some(seedValue) => seedValue.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
      case None => p.evaluate(PfsSolution(random.shuffle(p.jobs.toList))).asInstanceOf[PfsEvaluatedSolution]
    }
  }
  override def evaluate(problem: Problem): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val timeLimit = p.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    evaluate(p, stopCond)
  }
  override def evaluate(problem:Problem, stopCond: StoppingCondition):EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    def cost(solution: List[Int]) = p.evaluate(PfsSolution(solution)).asInstanceOf[PfsEvaluatedSolution]
    def neighbour(sol: List[Int]): List[Int] = NeighbourhoodOperator(random).SHIFT(sol)//forward or backward shift at random
    def acceptanceProbability(delta: Int, temperature: Double): Double = {
      scala.math.pow(2.71828,(-delta/temperature))
    }
     
    var evOldSolution = NaivePfsEvaluatedSolution(p)
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()
    def loop(old: PfsEvaluatedSolution, temp: Double, iter: Int): PfsEvaluatedSolution = {
      if((temp > temperatureLB) && stop.isNotSatisfied()) {
        if(iter == 1) {
          //initialize solution
          evOldSolution = initialSolution(p)
        } else {
          evOldSolution = old
        }
        var temperature = temp
        //generate random neighbouring solution
        val newSolution = neighbour(evOldSolution.solution.toList)
        //calculate its cost
        val evNewSolution = cost(newSolution)
          
        val delta = evNewSolution.value - evOldSolution.value
        //calculate acceptance probability
        val ap = acceptanceProbability(delta, temperature)
        val randomNo = random.nextDouble()
        if((delta <= 0) || (randomNo <= ap)) {
          evOldSolution = evNewSolution
        } 
        temperature = temperature / (1 + coolingRate*temperature)
        loop(evOldSolution, temperature, iter + 1)
      }
      else evOldSolution
    }
    loop(evOldSolution, temperatureUB, 1)
  }
  
}
