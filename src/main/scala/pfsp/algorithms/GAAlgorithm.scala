package pfsp.algorithms

import scala.Ordering
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.algorithms.Algorithm
import pfsp.problem.PfsProblem
import pfsp.solution.NaivePfsEvaluatedSolution
import pfsp.solution.PfsSolution
import pfsp.solution.PfsEvaluatedSolution
import pfsp.neighbourhood.NeighbourhoodOperator
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired

/**
 * @author Nemanja
 */

class GAAlgorithm(
    val popSize: Int, 
    val crossRate: Double, 
    val mutRate: Double, 
    val mutDecreaseFactor: Double, 
    val mutResetThreshold: Double,
    val seedOption: Option[PfsSolution]
    ) extends Algorithm {
  /**
   * Secondary constructors
   */
  def this(popSize: Int, seedOption: Option[PfsSolution]) {
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
  def initNEHSolution(p: PfsProblem): PfsEvaluatedSolution = {
    val nehAlgorithm = new NEHAlgorithm()
    nehAlgorithm.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
  }
  def initRandomSolution(p: PfsProblem): PfsEvaluatedSolution = {
    val randomAlgo = new RandomAlgorithm()
    randomAlgo.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
  }
  def initialSolution(p: PfsProblem): PfsEvaluatedSolution = {
    seed match {
      case Some(seedValue) => seedValue.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
      case None => initRandomSolution(p)
    }
  }
  override def evaluate(problem: Problem): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val timeLimit = p.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    evaluate(p, stopCond)
  }
  override def evaluate(problem:Problem, stopCond: StoppingCondition): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()

    def loop(pop: Array[PfsEvaluatedSolution], stats: (Double, Int, Int), mRate: Double, iter: Int): EvaluatedSolution = {
      if(stop.isNotSatisfied()) {
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
        // CROSSOVER
        val children = crossover(population)
        //MUTATION
        val mutation1 = mutation(children._1)
        val mutation2 = mutation(children._2)
        val child1 = p.evaluate(PfsSolution(mutation1)).asInstanceOf[PfsEvaluatedSolution]
        val child2 = p.evaluate(PfsSolution(mutation2)).asInstanceOf[PfsEvaluatedSolution]
        //UPDATE POPULATION
        population = replacement(population, Array(child1, child2))
        //UPDATE STATISTICS
        population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
        triple = calculateStatistics(population)
        mean = triple._1
        median = triple._2
        minimum = triple._3
        //UPDATE MUTATION RATE
        mutationRate = mutDecreaseFactor * mutationRate
        //threshold for mutation rate resetting
        if (minimum / mean > mutResetThreshold) mutationRate = mutRate
        //REPEAT
        loop(population, triple, mutationRate, iter + 1)
      }
      else {
        //RETURN BEST SOLUTION
        pop.minBy(_.value)
      }
    } //end def loop

    def crossover(pop: Array[PfsEvaluatedSolution]): (List[Int], List[Int]) = {
      //select both parents
      val parent1 = selectDetTour(pop, 2)
      val parent2 = selectDetTour(pop, 2)
      val parent1List = parent1.solution.toList
      val parent2List = parent2.solution.toList
      if (random.nextDouble() < crossRate) {
        crossoverQuad(parent1List, parent2List)
      } else {
        (parent1List, parent2List)
      }
    } // end def crossover

    def mutation(individual: List[Int]): List[Int] = {
      if (random.nextDouble() < mutRate) {
        // Run any of the two mutations
        if (random.nextDouble() < 0.5) {
          mutationINV(individual)
        } else {
          mutationSWAP(individual)
        }
      } else {
        individual
      }
    } // end def mutation

    // How the children are included in the population
    def replacement(pop: Array[PfsEvaluatedSolution], children: Array[PfsEvaluatedSolution]): Array[PfsEvaluatedSolution] = {
      // Remember previous best
      val prevBest = pop.minBy(_.value)
      var newPop = pop ++ children
      // Remove as many as needed
      while (newPop.size > pop.size) {
        // Select worst individual at random
        val individualToRemove = selectDetTour(newPop, 2, false)
        newPop = newPop.filterNot(_ == individualToRemove)
      }
      // Weak elitism
      // Introduce previous best if we deteriorate
      val newBest = newPop.minBy(_.value)
      if (newBest.value > prevBest.value) {
        // Tuple solution index, find max value, get its index
        val worstIndex = newPop.zipWithIndex.maxBy(_._1.value)._2
        newPop(worstIndex) = prevBest
      }
      newPop
    }

    //INITIALIZE POPULATION
    loop(Array(), (1.0, 1, 1), mutRate, 1)
  }

  def initRandom(p: PfsProblem, size: Int): Array[PfsEvaluatedSolution] = {
    def randomGenerate(jobs: List[Int]): PfsEvaluatedSolution = {
      p.evaluate(PfsSolution(random.shuffle(jobs))).asInstanceOf[PfsEvaluatedSolution]
    }
    val population = Array.ofDim[PfsEvaluatedSolution](size)
    val jobsList = p.jobs.toList
    for (i <- 0 until size) {
      population(i) = randomGenerate(jobsList)
    }
    population
  }
  def initSeedPlusRandom(p: PfsProblem, size: Int): Array[PfsEvaluatedSolution] = {
    val seedSol = initialSolution(p)//Random or provided seed in constructor, or in special evaluate function signature
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

  // Runs a deterministic tournament of a given size to select a individual
  def selectDetTour(pop: Array[PfsEvaluatedSolution], size: Int, bestWins: Boolean = true) = {
    var selected = getRandomIndividual(pop)
    for (i <- 1 to (size-1)) {
      val competitor = getRandomIndividual(pop)
      val shouldBeSelected =
        (bestWins && competitor.value < selected.value) || // Competitor is better
        (!bestWins && competitor.value > selected.value)   // Competitor is worse
      if (shouldBeSelected) {
        selected = competitor
      }
    }
    selected
  }

  def getRandomIndividual(pop: Array[PfsEvaluatedSolution]): PfsEvaluatedSolution = pop(random.nextInt(pop.length))

  def crossoverQuad(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    var point1 = 0
    var point2 = 0
    val popSize = parent1.size
    // Get two cut points at least three positions apart
    while (math.abs(point1 - point2) <= 2) {
      point1 = random.nextInt(popSize)
      point2 = random.nextInt(popSize)
    }
    // Swap values to order
    if (point1 > point2) {
      val temp = point1
      point1 = point2
      point2 = temp
    }
    val child1 = generateCrossoverQuad(parent1, parent2, point1, point2)
    val child2 = generateCrossoverQuad(parent2, parent1, point1, point2)
    (child1, child2)
  }

  def generateCrossoverQuad(parent1: List[Int], parent2: List[Int], point1: Int, point2: Int) : List[Int] = {
    val p1Begin = parent1.slice(0, point1+1)
    val p1Ending = parent1.slice(point2, parent1.size)

    val takenValues = (p1Begin ::: p1Ending).toSet

    val middleSize = point2-point1-1
    val p2Middle = parent2.filterNot(takenValues).take(middleSize)

    val child = p1Begin ::: p2Middle ::: p1Ending
    child
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
    NeighbourhoodOperator(random).SWAP(parent)
  }
  def mutationINV(parent: List[Int]): List[Int] = {
    NeighbourhoodOperator(random).INV(parent)
  }

  def mutationINS(parent: List[Int]): List[Int] = {
    NeighbourhoodOperator(random).BckINS(parent)
  }
  def calculateStatistics(sortedPopulation:Array[PfsEvaluatedSolution]):(Double,Int,Int) = {
    val makespans = sortedPopulation.map(_.value)
    val mean = makespans.sum.asInstanceOf[Double] / sortedPopulation.size
    val median = makespans.apply(sortedPopulation.size / 2 + 1)
    val minimum = makespans.reduceLeft(_ min _)
    (mean,median,minimum)
  }
}
