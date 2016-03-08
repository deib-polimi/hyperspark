package pfsp.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import pfsp.neighbourhood.NeighbourhoodOperator
import pfsp.problem.PfsProblem
import pfsp.solution.PfsSolution
import pfsp.solution.NaivePfsEvaluatedSolution
import pfsp.solution.PfsEvaluatedSolution
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired

/**
 * @author Nemanja
 */
class ISAAlgorithm(p: PfsProblem) extends SAAlgorithm(p) {
  
  protected var maxNotChangedTemp = 70  //pmax
  protected var maxNotChangedMS = 250  //qmax
  protected var maxItPerMS = 999999999    
  /**
   * A secondary constructor.
   */
  def this(p: PfsProblem, tUB: Double, tLB: Double, cRate: Double, mncTemp: Int, mncMS: Int, mitpMS: Int) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
    maxNotChangedTemp = mncTemp
    maxNotChangedMS = mncMS
    maxItPerMS = mitpMS
  }
  def this(p: PfsProblem, tUB: Double, tLB: Double, cRate: Double, mncTemp: Int, mncMS: Int, mitpMS: Int, seedOption: Option[Solution]) {
    this(p)
    temperatureUB = tUB
    temperatureLB = tLB
    coolingRate = cRate
    maxNotChangedTemp = mncTemp
    maxNotChangedMS = mncMS
    maxItPerMS = mitpMS
    seed = seedOption
  }
  def this(p: PfsProblem, seedOption: Option[Solution]) {
    this(p)
    seed = seedOption
  }

  override def evaluate(problem: Problem): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    //algorithm time limit
    val timeLimit = p.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    evaluate(p, stopCond)
  }
  override def evaluate(problem:Problem, stopCond: StoppingCondition):EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    def cost(list: List[Int]) = p.evaluate(PfsSolution(list)).asInstanceOf[PfsEvaluatedSolution]
    def neighbour(sol: List[Int]): List[Int] = NeighbourhoodOperator(random).SHIFT(sol) //forward or backward shift at random
    def acceptanceProbability(delta: Int, temperature: Double): Double = {
      scala.math.pow(2.71828, (-delta / temperature))
    } 
    val dummySol = NaivePfsEvaluatedSolution(p)
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()
    
    def loop(presSol: PfsEvaluatedSolution, bestGlob: PfsEvaluatedSolution, temp: Double, tIt: Int, iNCTemp: Int): PfsEvaluatedSolution = {
      var presentSol = presSol
      var bestLocal = presSol
      var bestGlobal = bestGlob
      var temperature = temp
      var iNotChangedTemp = iNCTemp//p
      var totalIterations = tIt
      
      if ((temperature >= temperatureLB) && (iNotChangedTemp < maxNotChangedTemp) && 
          stop.isNotSatisfied()) {
        //INITIALIZE
        if(totalIterations == 0) {
          presentSol = initialSolution(p)
          bestGlobal = presentSol
          bestLocal = presentSol
        }
        var iNotChangedMS = 0 //q
        var iterationsMS = 0  //j
        while ((iNotChangedMS < maxNotChangedMS) && (iterationsMS < maxItPerMS) && stop.isNotSatisfied()) {
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