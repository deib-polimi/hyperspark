package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import util.Timeout
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.solution.Solution
import util.ConsolePrinter
import util.RNG
import it.polimi.hyperh.solution.DummyEvaluatedSolution

/**
 * @author Nemanja
 */

class GAAlgorithm(
    val popSize: Int, 
    val crossRate: Double, 
    val mutRate: Double, 
    val mutDecreaseFactor: Double, 
    val mutResetThreshold: Double,
    val seedOption: Option[Solution]
    ) extends Algorithm {
  /**
   * Secondary constructors
   */
  def this(popSize: Int, seedOption: Option[Solution]) {
    this(popSize, 1.0, 0.8, 0.99, 0.95, seedOption)
  }
  def this(popSize: Int) {
    this(popSize, 1.0, 0.8, 0.99, 0.95, None)
  }
  def this() {
    //popSize:30, crossRate:1.0, mutRate: 0.8, mutDecreaseFactor: 0.99, mutResetThreshold: 0.95
    this(30, 1.0, 0.8, 0.99, 0.95, None)
  }
  seed = seedOption
  def initNEHSolution(p: Problem) = {
    val nehAlgorithm = new NEHAlgorithm()
    nehAlgorithm.evaluate(p)
  }
  def initialSolution(p: Problem): EvaluatedSolution = {
    seed match {
      case Some(seedValue) => seedValue.evaluate(p)
      case None => initNEHSolution(p)
    }
  }
  override def evaluate(p: Problem): EvaluatedSolution = {
    val timeLimit = p.numOfMachines*(p.numOfJobs/2.0)*60//termination is n*(m/2)*60 milliseconds
    evaluate(p, timeLimit)
  }
  override def evaluate(p:Problem, timeLimit: Double): EvaluatedSolution = {
    //INITIALIZE POPULATION
    var child1 = DummyEvaluatedSolution(p)
    var child2 = DummyEvaluatedSolution(p)
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    def loop(pop: Array[EvaluatedSolution], stats: (Double, Int, Int), mRate: Double, iter: Int): EvaluatedSolution = {
      if(Timeout.notTimeout(expireTimeMillis)) {
          var population = pop
          var triple = stats
          var mean = triple._1
          var median = triple._2
          var minimum = triple._3
          var medianIndex = popSize / 2 + 1//population.map(x => x.value).indexOf(median)
          var mutationRate = mRate
        if(iter == 1) {
          //INITIALIZE POPULATION
          population = initSeedPlusRandom(p, popSize)
          population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
          //calculate population statistics
          triple = calculateStatistics(population)
          mean = triple._1
          median = triple._2
          minimum = triple._3
          medianIndex = popSize / 2 + 1 //population.map(x => x.value).indexOf(median)
          mutationRate = mutRate
        }
        //CROSSOVER
        val randomNo = random.nextDouble()
        if (randomNo < crossRate) {
          //select parent1 using fitness_rank distribution
          val parent1 = population(medianIndex+random.nextInt(popSize-medianIndex))
          //select parent2 using uniform distribution
          val parent2 = population(random.nextInt(popSize))
          val children = crossoverC1(parent1.solution.toList, parent2.solution.toList)
          child1 = p.evaluate(Solution(children._1))
          child2 = p.evaluate(Solution(children._2))
        }
        //MUTATION
        if (randomNo < mutRate) {
          val mutation1 = mutationSWAP(child1.solution.toList)
          val mutation2 = mutationSWAP(child2.solution.toList)
          child1 = p.evaluate(Solution(mutation1))
          child2 = p.evaluate(Solution(mutation2))
        }
        //UPDATE POPULATION
        //delete sequence from unfit members, whose makespan value is below the median
        val index1 = random.nextInt(medianIndex)
        val index2 = random.nextInt(medianIndex)
        //insert new members into population (at the same time deleting old members)
        population(index1) = child1
        population(index2) = child2
        //UPDATE STATISTICS
        population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
        triple = calculateStatistics(population)
        mean = triple._1
        median = triple._2
        minimum = triple._3
        //UPDATE MUTATION RATE
        mutationRate = mutDecreaseFactor * mutationRate
        //threshold for mutation rate reseting
        if (minimum / mean > mutResetThreshold) mutationRate = mutRate
        //REPEAT
        loop(population, triple, mutationRate, iter + 1)
      }
      else {
        //RETURN BEST SOLUTION
        pop.minBy(_.value)
      }
    } //end def loop
    loop(Array(), (1.0, 1, 1), mutRate, 1)
  }

  def initRandom(p: Problem, size: Int): Array[EvaluatedSolution] = {
    def randomGenerate(jobs: List[Int]): EvaluatedSolution = {
      p.evaluate(Solution(random.shuffle(jobs)))
    }
    val population = Array.ofDim[EvaluatedSolution](size)
    val jobsList = p.jobs.toList
    for (i <- 0 until size) {
      population(i) = randomGenerate(jobsList)
    }
    population
  }
  def initSeedPlusRandom(p: Problem, size: Int): Array[EvaluatedSolution] = {
    val seedSol = initialSolution(p)//NEH or provided seed in constructor, or in special evaluate function signature
    val population = Array(seedSol) ++ initRandom(p, size-1)
    population
  }

  def crossoverLOX(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    val size = parent1.size
    val firstPoint = random.nextInt(size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + random.nextInt(size - firstPoint) //[firstPoint+1,n]
    val p1Remove = parent2.drop(firstPoint).take(secondPoint - firstPoint)
    val p2Remove = parent1.drop(firstPoint).take(secondPoint - firstPoint)
    val p1Filtered = parent1.filterNot(p1Remove.toSet)
    val p2Filtered = parent2.filterNot(p2Remove.toSet)
    val p1Reconstructed = p1Filtered.take(firstPoint) ::: p1Remove ::: p1Filtered.drop(firstPoint)
    val p2Reconstructed = p2Filtered.take(firstPoint) ::: p2Remove ::: p2Filtered.drop(firstPoint)
    (p1Reconstructed, p2Reconstructed)
  }
  def crossoverPMX(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    val firstPoint = random.nextInt(parent1.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + random.nextInt(parent1.size - firstPoint) //[firstPoint+1,n]
    val child1Part1 = parent1.take(firstPoint)
    val child1Part2 = parent1.drop(firstPoint).take(secondPoint - firstPoint)
    val child1Part3 = parent1.drop(secondPoint)
    val child2Part1 = parent2.take(firstPoint)
    val child2Part2 = parent2.drop(firstPoint).take(secondPoint - firstPoint)
    val child2Part3 = parent2.drop(secondPoint)
    val mappings1 = child1Part2 zip child2Part2
    val mappings2 = child2Part2 zip child1Part2
    def applyMapping(list: List[Int], mappings: List[(Int, Int)], result: List[Int]): List[Int] = list match {
      case List() => result
      case head :: tail => mappings.filter(y => y._1 == head).toList match {
        case List()  => applyMapping(tail, mappings, result ::: List(head))
        case m :: ms => applyMapping(m._2 :: tail, mappings, result)
      }
    }
    val child1 = applyMapping(child1Part1, mappings2, List()) ::: child2Part2 ::: applyMapping(child1Part3, mappings2, List())
    val child2 = applyMapping(child2Part1, mappings1, List()) ::: child1Part2 ::: applyMapping(child2Part3, mappings1, List())
    (child1.toList, child2.toList)
  }

  def crossoverC1(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    val crossoverPoint = 1 + random.nextInt(parent1.size - 2) //[1,n-2]
    val p1Same = parent1.take(crossoverPoint) //crossoverPoint elements remains the same, fill the rest
    val p2Same = parent2.take(crossoverPoint)
    val p1Add = parent2.filterNot(p1Same.toSet)
    val p2Add = parent1.filterNot(p2Same.toSet)
    val child1 = p1Same ::: p1Add
    val child2 = p2Same ::: p2Add
    (child1, child2)
  }
  def crossoverNABEL(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    val child1 = Array.ofDim[Int](parent1.size)
    val child2 = Array.ofDim[Int](parent2.size)
    for (i <- 0 until parent1.size) {
      child1(i) = parent1(parent2(i) - 1)
      child2(i) = parent2(parent1(i) - 1)
    }
    (child1.toList, child2.toList)
  }
  def mutationSWAP(parent: List[Int]): List[Int] = {
    NeighbourhoodSearch(random).SWAP(parent)
  }
  def mutationINV(parent: List[Int]): List[Int] = {
    NeighbourhoodSearch(random).INV(parent)
  }

  def mutationINS(parent: List[Int]): List[Int] = {
    NeighbourhoodSearch(random).BckINS(parent)
  }
  def calculateStatistics(sortedPopulation:Array[EvaluatedSolution]):(Double,Int,Int) = {
    val makespans = sortedPopulation.map(_.value)
    val mean = makespans.sum.asInstanceOf[Double] / sortedPopulation.size
    val median = makespans.apply(sortedPopulation.size / 2 + 1)
    val minimum = makespans.reduceLeft(_ min _)
    (mean,median,minimum)
  }
}
