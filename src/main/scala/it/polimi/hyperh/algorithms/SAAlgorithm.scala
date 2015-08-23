package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import util.Timeout

/**
 * @author Nemanja
 */
object SAAlgorithm {
  def apply(p: Problem, temperatureUB: Double, temperatureLB: Double, coolingRate: Double): EvaluatedSolution = {
    evaluate(p, temperatureUB, temperatureLB, coolingRate)
  }
  //temperatureUB: 1.0, temperatureLB: 0.00001, coolingRate: 0.9
  def evaluate(p: Problem, temperatureUB: Double, temperatureLB: Double, coolingRate: Double): EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
    def cost(solution: List[Int]) = p.evaluatePartialSolution(solution.toArray, p.jobTimesMatrix, initEndTimesMatrix)
    def neighbour(sol: List[Int]): List[Int] = GAAlgorithm.mutationSWAP(sol)//swapping two elements at random
    def acceptanceProbability(oldCost: Int, newCost: Int, temperature: Double): Double = {
      scala.math.pow(2.71828,((newCost-oldCost)/temperature))
    }
    
    var temperature = temperatureUB
    //generate random solution
    var oldSolution = Random.shuffle(p.jobs.toList)
    //calculate its cost
    var evOldSolution = cost(oldSolution)
    var bestSolution = evOldSolution
    
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while ((temperature > temperatureLB) && Timeout.notTimeout(expireTimeMillis)) {
      //generate random neighbouring solution
      val newSolution = neighbour(oldSolution)
      //calculate its cost
      val evNewSolution = cost(newSolution)
      if(evNewSolution.value > bestSolution.value) 
        bestSolution = evNewSolution
      //calculate acceptance probability
      val ap = acceptanceProbability(evOldSolution.value, evNewSolution.value, temperature)
      val randomNo = Random.nextDouble()
      //compare them
      if(ap > randomNo) {
        oldSolution = newSolution
        evOldSolution = evNewSolution
      }
      temperature = temperature * coolingRate
    }
    bestSolution
  }
}