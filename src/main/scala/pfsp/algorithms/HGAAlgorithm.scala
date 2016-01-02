package pfsp.algorithms

import scala.Ordering
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired
import pfsp.solution.PfsSolution
import pfsp.solution.PfsEvaluatedSolution
import pfsp.problem.PfsProblem

/**
 * @author Nemanja
 */
class HGAAlgorithm(
    p: PfsProblem, 
    popSize: Int, 
    prob: Double, 
    coolingRate: Double,
    seedOption: Option[PfsSolution]
    ) extends GAAlgorithm(popSize, seedOption) {
  /**
   * A secondary constructor.
   */
  def this(p: PfsProblem, seedOption: Option[PfsSolution]) {
    this(p, 40, 0.1, 0.95, seedOption)
  }
  def this(p: PfsProblem) {
    //popSize: 40, prob: 0.1, coolingRate: 0.95
    this(p, 40, 0.1, 0.95, None)
  }
  var temperatureUB:Double = 2000.0 //dummy initialization
  seed = seedOption
  
  def acceptanceProbability(delta: Int, temperature: Double): Double = {
    scala.math.min(1.0, scala.math.pow(2.71828, (-delta / temperature)))
  }
  override def evaluate(problem: Problem): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val timeLimit = p.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    evaluate(p, stopCond)
  }
  override def evaluate(problem:Problem, stopCond: StoppingCondition): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    //INITIALIZE POPULATION
    var population = initSeedPlusRandom(p, popSize)
    var bestSolution = population.minBy(_.value)
    var worstSolution = population.maxBy(_.value)
    val delta = worstSolution.value - bestSolution.value
    temperatureUB = -delta / scala.math.log(prob)
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()
    
    while (stop.isNotSatisfied()) {
      //DIVIDE POPULATION IN 4 SUBPOPULATION
      val subPopSize = popSize / 4
      var subpopulation1 = population.take(subPopSize)
      var subpopulation2 = population.drop(subPopSize).take(subPopSize)
      var subpopulation3 = population.drop(2*subPopSize).take(subPopSize)
      var subpopulation4 = population.drop(3*subPopSize).take(subPopSize)
      //CROSSOVER
      subpopulation1 = crossover(p, subpopulation1, bestSolution, crossoverLOX, stop)
      subpopulation2 = crossover(p, subpopulation2, bestSolution, crossoverPMX, stop)
      subpopulation3 = crossover(p, subpopulation3, bestSolution, crossoverC1, stop)
      subpopulation4 = crossover(p, subpopulation4, bestSolution, crossoverNABEL, stop)
      //UPDATE POPULATION
      population = subpopulation1 ++ subpopulation2 ++ subpopulation3 ++ subpopulation4
      bestSolution = List(population.minBy(_.value), bestSolution).minBy(_.value)
      //METROPOLIS MUTATION
      population = metropolis(p, population, stop)
      bestSolution = List(population.minBy(_.value), bestSolution).minBy(_.value)
      
    } //endwhile
    //RETURN BEST SOLUTION
    bestSolution
  }
  def crossover(p: PfsProblem, subpopulation: Array[PfsEvaluatedSolution], bestSolution: PfsEvaluatedSolution,
      operator: (List[Int], List[Int]) => (List[Int], List[Int]), stopCond: StoppingCondition): Array[PfsEvaluatedSolution] = {
    var newPopulation = subpopulation
    val Ps = subpopulation.size
    //parent1 uses bestSolution
    val parent1 = bestSolution
    //select parent2 using uniform distribution
    val parent2 = subpopulation(this.random.nextInt(Ps))
    //apply crossover operator
    val children = operator(parent1.solution.toList, parent2.solution.toList)
    val child1 = p.evaluate(PfsSolution(children._1)).asInstanceOf[PfsEvaluatedSolution]
    val child2 = p.evaluate(PfsSolution(children._2)).asInstanceOf[PfsEvaluatedSolution]
    newPopulation = newPopulation ++ Array(child1) ++ Array(child2)
    newPopulation = newPopulation.sortBy[Int](_.value)(Ordering.Int).take(Ps)
    newPopulation
  }
  def metropolis(p: PfsProblem, population: Array[PfsEvaluatedSolution], stopCond: StoppingCondition) = {

    var evOldPopulation = population
    var temperature = temperatureUB
    var iter = 0
    //repeat metropolis sample n times at current temperature
    while (iter < p.numOfJobs && stopCond.isNotSatisfied()) {
      for(i <- 0 until evOldPopulation.size) {
        var newSolution: List[Int] = List()
        var localBest = evOldPopulation(i)
        var runs = 0
        while((runs < p.numOfJobs) && stopCond.isNotSatisfied()) {
          //START METROPOLIS SAMPLE ITERATION
          //generate random neighbouring solution
          //i mod 4, 0,1: swap, 2: inv, 3: ins
          val op = i % 4
          if(op == 0 || op == 1)
            newSolution = mutationSWAP(evOldPopulation(i).solution.toList)
          else if(op == 2)
            newSolution = mutationINV(evOldPopulation(i).solution.toList)
          else
            newSolution = mutationINS(evOldPopulation(i).solution.toList)
          //calculate its cost
          val evNewSolution = p.evaluate(PfsSolution(newSolution)).asInstanceOf[PfsEvaluatedSolution]
            
          val delta = evNewSolution.value - evOldPopulation(i).value
          //calculate acceptance probability
          val ap = acceptanceProbability(delta, temperature)
          val randomNo = random.nextDouble()
          
          if ((delta <= 0) || (randomNo <= ap)) {
            evOldPopulation(i) = evNewSolution
          }
          //if we found better solution in the neighbourhood, update localBest
          localBest = List(evNewSolution, localBest).minBy(_.value)
          runs = runs + 1
          //END METROPOLIS SAMPLE ITERATION
        }
        evOldPopulation(i) = localBest
        temperature = coolingRate*temperature
      }
      iter = iter + 1
    }//end while
    evOldPopulation
  }
}