package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import util.Timeout
import it.polimi.hyperh.neighbourhood.NeighbourhoodOperator
import it.polimi.hyperh.solution.Solution
import util.RNG
import it.polimi.hyperh.solution.DummyEvaluatedSolution

/**
 * @author Nemanja
 */
class SAAlgorithm(p: Problem) extends Algorithm {

  var temperatureUB: Double = p.sumJobTimesMatrix() / (5.0*p.numOfJobs*p.numOfMachines)
  var temperatureLB: Double = 1.0
  var iterations: Double = scala.math.max(3300*scala.math.log(p.numOfJobs)+7500*scala.math.log(p.numOfMachines)-18250, 2000)
  var coolingRate: Double = (temperatureUB-temperatureLB)/((iterations-1)*temperatureUB*temperatureLB)
  /**
   * A secondary constructor.
   */
  def this(p: Problem, tUB: Double, tLB: Double, cRate: Double) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
  }
  def this(p: Problem, tUB: Double, tLB: Double, cRate: Double, seedOption: Option[Solution]) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
    seed = seedOption
  }

  def this(p: Problem, seedOption: Option[Solution]) {
    this(p)
    seed = seedOption
  }

  def initialSolution(p: Problem): EvaluatedSolution = {
    seed match {
      case Some(seedValue) => seedValue.evaluate(p)
      case None => p.evaluate(Solution(random.shuffle(p.jobs.toList)))
    }
  }
  override def evaluate(p: Problem): EvaluatedSolution = {
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    evaluate(p, timeLimit)
  }
  override def evaluate(p:Problem, timeLimit: Double):EvaluatedSolution = {
    def cost(solution: List[Int]) = p.evaluate(Solution(solution))
    def neighbour(sol: List[Int]): List[Int] = NeighbourhoodOperator(random).SHIFT(sol)//forward or backward shift at random
    def acceptanceProbability(delta: Int, temperature: Double): Double = {
      scala.math.pow(2.71828,(-delta/temperature))
    }
     
    var evOldSolution = DummyEvaluatedSolution(p)
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    def loop(old: EvaluatedSolution, temp: Double, iter: Int): EvaluatedSolution = {
      if((temp > temperatureLB) && Timeout.notTimeout(expireTimeMillis)) {
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
