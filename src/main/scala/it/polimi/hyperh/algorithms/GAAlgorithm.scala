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

class GAAlgorithm(val popSize: Int, val crossRate: Double, val mutRate: Double, val mutDecreaseFactor: Double, val mutResetThreshold: Double) extends Algorithm {
  /**
   * A secondary constructor.
   */
  def this() {
    //popSize:30, crossRate:1.0, mutRate: 0.8, mutDecreaseFactor: 0.99, mutResetThreshold: 0.95
    this(30, 1.0, 0.8, 0.99, 0.95)
  }
  override def evaluate(p: Problem): EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
    //INITIALIZE POPULATION
    var population = GAAlgorithm.initNEHplusRandom(p, popSize, initEndTimesMatrix)
    population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
    //calculate population statistics
    var triple = GAAlgorithm.calculateStatistics(population)
    var mean = triple._1
    var median = triple._2
    var minimum = triple._3
    var medianIndex = population.map(x => x.value).indexOf(median)
    var mutationRate = mutRate
    var child1 = new EvaluatedSolution(999999999, p.jobs)//dummy initialization
    var child2 = new EvaluatedSolution(999999999, p.jobs)//dummy initialization
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      val randomNo = Random.nextDouble()
      if (randomNo < crossRate) {                          //CROSSOVER
        //select parent1 using fitness_rank distribution
        val parent1 = population(medianIndex+Random.nextInt(popSize-medianIndex))
        //select parent2 using uniform distribution
        val parent2 = population(Random.nextInt(popSize))
        val children = GAAlgorithm.crossoverC1(parent1.solution.toList, parent2.solution.toList)
        child1 = Problem.evaluate(p, new Solution(children._1), initEndTimesMatrix)
        child2 = Problem.evaluate(p, new Solution(children._2), initEndTimesMatrix)
      }
      if (randomNo < mutRate) {                            //MUTATION
        val mutation1 = GAAlgorithm.mutationSWAP(child1.solution.toList)
        val mutation2 = GAAlgorithm.mutationSWAP(child2.solution.toList)
        child1 = Problem.evaluate(p, new Solution(mutation1), initEndTimesMatrix)
        child2 = Problem.evaluate(p, new Solution(mutation2), initEndTimesMatrix)
      }
      //UPDATE POPULATION
      //delete sequence from unfit members, whose makespan value is below the median
      val index1 = Random.nextInt(medianIndex)
      val index2 = Random.nextInt(medianIndex)
      //insert new members into population (at the same time deleting old members)
      population(index1) = child1
      population(index2) = child2
      //UPDATE STATISTICS
      population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
      triple = GAAlgorithm.calculateStatistics(population)
      mean = triple._1
      median = triple._2
      minimum = triple._3
      //UPDATE MUTATION RATE
      mutationRate = mutDecreaseFactor * mutationRate
      //threshold for mutation rate reseting
      if (minimum / mean > mutResetThreshold) mutationRate = mutRate
    } //endwhile
    //RETURN BEST SOLUTION
    population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
    population(popSize-1)//return best solution
  }
}
object GAAlgorithm {
  
  def initRandom(p: Problem, size: Int, initEndTimesMatrix: Array[Array[Int]]): Array[EvaluatedSolution] = {
    def randomGenerate(jobs: List[Int]): EvaluatedSolution = {
      p.evaluatePartialSolution(Random.shuffle(jobs).toArray, p.jobTimesMatrix, initEndTimesMatrix)
    }
    val population = Array.ofDim[EvaluatedSolution](size)
    for (i <- 0 until size) {
      population(i) = randomGenerate(p.jobs.toList)
    }
    population
  }

  def initNEHplusRandom(p: Problem, size: Int, initEndTimesMatrix: Array[Array[Int]]): Array[EvaluatedSolution] = {
    val nehAlgorithm = new NEHAlgorithm()
    val nehEvSolution = nehAlgorithm.evaluate(p)
    val population = Array(nehEvSolution) ++ initRandom(p, size, initEndTimesMatrix)
    population
  }

  def crossoverLOX(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    val size = parent1.size
    val firstPoint = Random.nextInt(size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(size - firstPoint) //[firstPoint+1,n]
    val p1Remove = parent2.drop(firstPoint).take(secondPoint - firstPoint)
    val p2Remove = parent1.drop(firstPoint).take(secondPoint - firstPoint)
    val p1Filtered = parent1.filterNot(p1Remove.toSet)
    val p2Filtered = parent2.filterNot(p2Remove.toSet)
    val p1Reconstructed = p1Filtered.take(firstPoint) ::: p1Remove ::: p1Filtered.drop(firstPoint)
    val p2Reconstructed = p2Filtered.take(firstPoint) ::: p2Remove ::: p2Filtered.drop(firstPoint)
    (p1Reconstructed, p2Reconstructed)
  }
  def crossoverPMX(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    val firstPoint = Random.nextInt(parent1.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent1.size - firstPoint) //[firstPoint+1,n]
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
        case m :: ms => applyMapping(tail, mappings, result ::: List(m._2))
      }
    }
    val child1 = applyMapping(child1Part1, mappings2, List()) ::: child2Part2 ::: applyMapping(child1Part3, mappings2, List())
    val child2 = applyMapping(child2Part1, mappings1, List()) ::: child1Part2 ::: applyMapping(child2Part3, mappings1, List())
    (child1.toList, child2.toList)
  }

  def crossoverC1(parent1: List[Int], parent2: List[Int]): (List[Int], List[Int]) = {
    val crossoverPoint = 1 + Random.nextInt(parent1.size - 2) //[1,n-2]
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
    NeighbourhoodSearch.SWAP(parent)
  }
  def mutationINV(parent: List[Int]): List[Int] = {
    NeighbourhoodSearch.INV(parent)
  }

  def mutationINS(parent: List[Int]): List[Int] = {
    NeighbourhoodSearch.BckINS(parent)
  }
  def calculateStatistics(sortedPopulation:Array[EvaluatedSolution]):(Double,Int,Int) = {
    val makespans = sortedPopulation.map(_.value)
    val mean = makespans.sum.asInstanceOf[Double] / sortedPopulation.size
    val median = makespans.apply(sortedPopulation.size / 2 + 1)
    val minimum = makespans.reduceLeft(_ min _)
    (mean,median,minimum)
  }
 
}