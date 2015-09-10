package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import util.Timeout
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.solution.Solution

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
  override def evaluate(p: Problem): EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
    def cost(solution: List[Int]) = Problem.evaluate(p, new Solution(solution))
    def neighbour(sol: List[Int]): List[Int] = NeighbourhoodSearch.SHIFT(sol)//forward or backward shift at random
    def acceptanceProbability(delta: Int, temperature: Double): Double = {
      scala.math.pow(2.71828,(-delta/temperature))
    }
    
    var temperature = temperatureUB
    //generate random solution
    var oldSolution = Random.shuffle(p.jobs.toList)
    //calculate its cost
    var evOldSolution = cost(oldSolution)
    
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while ((temperature > temperatureLB) && Timeout.notTimeout(expireTimeMillis)) {
      //generate random neighbouring solution
      val newSolution = neighbour(oldSolution)
      //calculate its cost
      val evNewSolution = cost(newSolution)
        
      val delta = evNewSolution.value - evOldSolution.value
      //calculate acceptance probability
      val ap = acceptanceProbability(delta, temperature)
      val randomNo = Random.nextDouble()
      if((delta <= 0) || (randomNo <= ap)) {
        oldSolution = newSolution
        evOldSolution = evNewSolution
      } 
      temperature = temperature / (1 + coolingRate*temperature)
    }
    evOldSolution
  }
}
