package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import util.Timeout
import it.polimi.hyperh.neighbourhood.NeighbourhoodOperator
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.DummyEvaluatedSolution

/**
 * @author Nemanja
 */
class ISAAlgorithm(p: Problem) extends SAAlgorithm(p) {
  
  protected var maxNotChangedTemp = 70  //pmax
  protected var maxNotChangedMS = 250  //qmax
  protected var maxItPerMS = 999999999    
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
  def this(p: Problem, tUB: Double, tLB: Double, cRate: Double, mncTemp: Int, mncMS: Int, mitpMS: Int, seedOption: Option[Solution]) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
    maxNotChangedTemp = mncTemp
    maxNotChangedMS = mncMS
    maxItPerMS = mitpMS
    seed = seedOption
  }
  def this(p: Problem, seedOption: Option[Solution]) {
    this(p)
    seed = seedOption
  }

  override def evaluate(p: Problem): EvaluatedSolution = {
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    evaluate(p, timeLimit)
  }
  override def evaluate(p:Problem, timeLimit: Double):EvaluatedSolution = {
    def cost(list: List[Int]) = p.evaluate(Solution(list))
    def neighbour(sol: List[Int]): List[Int] = NeighbourhoodOperator(random).SHIFT(sol) //forward or backward shift at random
    def acceptanceProbability(delta: Int, temperature: Double): Double = {
      scala.math.pow(2.71828, (-delta / temperature))
    } 
    val dummySol = DummyEvaluatedSolution(p)
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    def loop(presSol: EvaluatedSolution, bestGlob: EvaluatedSolution, temp: Double, tIt: Int, iNCTemp: Int): EvaluatedSolution = {
      var presentSol = presSol
      var bestLocal = presSol
      var bestGlobal = bestGlob
      var temperature = temp
      var iNotChangedTemp = iNCTemp//p
      var totalIterations = tIt
      
      if ((temperature >= temperatureLB) && (iNotChangedTemp < maxNotChangedTemp) && 
          Timeout.notTimeout(expireTimeMillis)) {
        //INITIALIZE
        if(totalIterations == 0) {
          presentSol = initialSolution(p)
          bestGlobal = presentSol
          bestLocal = presentSol
        }
        var iNotChangedMS = 0 //q
        var iterationsMS = 0  //j
        while ((iNotChangedMS < maxNotChangedMS) && (iterationsMS < maxItPerMS) && Timeout.notTimeout(expireTimeMillis)) {
          iterationsMS = iterationsMS + 1//step1
          //START METROPOLIS SAMPLE ITERATION
          //generate random neighbouring solution, step1
          val neighbourSol = neighbour(presentSol.solution.toList)
          //calculate its cost
          val evNeighbourSol = cost(neighbourSol)
  
          val delta = evNeighbourSol.value - presentSol.value
          //calculate acceptance probability
          val ap = acceptanceProbability(delta, temperature)
          val randomNo = random.nextDouble()//step2
          
          if ((delta <= 0) || (randomNo <= ap)) {
            presentSol = evNeighbourSol//step3 : accept neighbour solution
            //if we found better solution in the neighbourhood, update the bestLocal
            if(evNeighbourSol.value < bestLocal.value) {
              bestLocal = evNeighbourSol
              iNotChangedMS = 0                  //q=0
            } else {
              iNotChangedMS = iNotChangedMS + 1  //q=q+1
            }
          }//else go to step1
          //END METROPOLIS SAMPLE ITERATION
        }//step5 put in while
        totalIterations = totalIterations + iterationsMS
        //UPDATING SEQUENCE OF PRESENT SOLUTION
        //if metropolis sample found better solution in this temperature iteration, update best solution
        if(bestLocal.value < bestGlobal.value) {//step6
          bestGlobal = bestLocal
          iNotChangedTemp = 0                      //p=0
        } else {
          iNotChangedTemp = iNotChangedTemp + 1    //p=p+1
        }
        temperature = temperature / (1 + coolingRate * temperature)
        //REPEAT
        loop(presentSol, bestGlobal, temperature, totalIterations, iNotChangedTemp)
      }
      else bestGlobal
    }
    loop(dummySol, dummySol, temperatureUB, 0, 0) 
  }

}