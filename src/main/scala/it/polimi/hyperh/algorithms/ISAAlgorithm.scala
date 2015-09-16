package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import util.Timeout
import it.polimi.hyperh.search.NeighbourhoodSearch

/**
 * @author Nemanja
 */
class ISAAlgorithm(p: Problem) extends SAAlgorithm(p) {
  
  var maxNotChangedTemp = 70  //pmax
  var maxNotChangedMS = 250  //qmax
  var maxItPerMS = 999999999    
  /**
   * A secondary constructor.
   */
  def this(p: Problem, tUB: Double, tLB: Double, cRate: Double, mncTemp: Int, mncMS: Int, mitpMS: Int) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
    maxNotChangedTemp = mncTemp
    maxNotChangedMS = mncMS
    maxItPerMS = mitpMS
  }
  def this(p: Problem, tUB: Double, tLB: Double, cRate: Double, mncTemp: Int, mncMS: Int, mitpMS: Int, seedOption: Option[EvaluatedSolution]) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
    maxNotChangedTemp = mncTemp
    maxNotChangedMS = mncMS
    maxItPerMS = mitpMS
    seed = seedOption
  }
  def this(p: Problem, seedOption: Option[EvaluatedSolution]) {
    this(p)
    seed = seedOption
  }
  override def evaluate(p: Problem): EvaluatedSolution = {
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    evaluate(p, timeLimit)
  }
  override def evaluate(p:Problem, timeLimit: Double):EvaluatedSolution = {
    def cost(solution: List[Int]) = p.evaluatePartialSolution(solution.toArray)
    def neighbour(sol: List[Int]): List[Int] = NeighbourhoodSearch.SHIFT(sol) //forward or backward shift at random
    def acceptanceProbability(delta: Int, temperature: Double): Double = {
      scala.math.pow(2.71828, (-delta / temperature))
    } 
    val dummySol = new EvaluatedSolution(999999999, p.jobs)//dummy initialization
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    def loop(evOldSolTrack: EvaluatedSolution, bestSol: EvaluatedSolution, temp: Double, tIt: Int, iNCTemp: Int): EvaluatedSolution = {
      var evOldSolutionTrack = evOldSolTrack
      var oldSolutionTrack = evOldSolutionTrack.solution.toList
      var bestSolution = bestSol
      var temperature = temp
      var iNotChangedTemp = iNCTemp
      var totalIterations = tIt
      if(totalIterations == 0) {
        //initialize solution
        evOldSolutionTrack = initialSolution(p)
        oldSolutionTrack = evOldSolutionTrack.solution.toList
        //variable for rembembering best solutions
        bestSolution = evOldSolutionTrack
      }
      
      if ((temperature >= temperatureLB) && (iNotChangedTemp < maxNotChangedTemp) && 
          Timeout.notTimeout(expireTimeMillis)) {
        var iNotChangedMS = 0 //q
        var iterationsMS = 0  //j
        while ((iNotChangedMS < maxNotChangedMS) && (iterationsMS < maxItPerMS) && Timeout.notTimeout(expireTimeMillis)) {
          iterationsMS = iterationsMS + 1
          //START METROPOLIS SAMPLE ITERATION
          //generate random neighbouring solution
          val newSolutionTrack = neighbour(oldSolutionTrack)
          //calculate its cost
          val evNewSolutionTrack = cost(newSolutionTrack)
  
          val delta = evNewSolutionTrack.value - evOldSolutionTrack.value
          //calculate acceptance probability
          val ap = acceptanceProbability(delta, temperature)
          val randomNo = Random.nextDouble()
          
          if ((delta <= 0) || (randomNo <= ap)) {
            oldSolutionTrack = newSolutionTrack
            evOldSolutionTrack = evNewSolutionTrack
          }
          //if we found better solution in the neighbourhood, update the oldSolution
          if(evNewSolutionTrack.value < evOldSolutionTrack.value) {
            evOldSolutionTrack = evNewSolutionTrack
            iNotChangedMS = 0                  //q=0
          } else {
            iNotChangedMS = iNotChangedMS + 1  //q=q+1
          }
          //END METROPOLIS SAMPLE ITERATION
        }
        totalIterations = totalIterations + iterationsMS
        //UPDATING SEQUENCE OF PRESENT SOLUTION
        //if metropolis sample found better solution in this temperature iteration, update best solution
        if(evOldSolutionTrack.value < bestSolution.value) {
          bestSolution = evOldSolutionTrack
          iNotChangedTemp = 0                      //p=0
        } else {
          iNotChangedTemp = iNotChangedTemp + 1    //p=p+1
        }
        temperature = temperature / (1 + coolingRate * temperature)
        //REPEAT
        loop(evOldSolutionTrack, bestSolution, temperature, totalIterations, iNotChangedTemp)
      }
      //println("ISA total number of iterations: "+totalIterations)
      else bestSolution
    }
    loop(dummySol, dummySol, temperatureUB, 0, 0) 
  }

}