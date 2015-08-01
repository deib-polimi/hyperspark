package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import util.Timeout

/**
 * @author Nemanja
 */
object GAAlgorithm {
  def apply(p: Problem, popSize: Int, crossRate: Double, mutRate: Double, mutDecreaseFactor: Double, mutResetThreshold: Double): EvaluatedSolution = {
    evaluate(p, popSize, crossRate, mutRate, mutDecreaseFactor, mutResetThreshold)
  }
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
    val nehEvSolution = NEHAlgorithm.evaluate(p)
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
    val firstPoint = Random.nextInt(parent.size) //[0,n-1]
    var secondPoint = firstPoint
    while (secondPoint == firstPoint) { //second point must be different than first
      secondPoint = Random.nextInt(parent.size)
    }
    val mutated = parent.toArray
    val tmp = mutated(firstPoint)
    mutated(firstPoint) = mutated(secondPoint)
    mutated(secondPoint) = tmp
    mutated.toList
  }
  def mutationINV(parent: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(parent.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent.size - firstPoint) //[firstPoint+1,n]
    val mutatedPart1 = parent.take(firstPoint)
    val mutatedPart2 = parent.drop(firstPoint).take(secondPoint - firstPoint).reverse
    val mutatedPart3 = parent.drop(secondPoint)
    mutatedPart1 ::: mutatedPart2 ::: mutatedPart3
  }

  def mutationINS(parent: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(parent.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent.size - firstPoint - 1) //[firstPoint+1,n]
    println(firstPoint + "," + secondPoint)
    val mutatedPart1 = parent.take(firstPoint)
    val mutatedPart2 = parent.drop(secondPoint).take(1)
    val mutatedPart3 = parent.drop(firstPoint).filterNot(mutatedPart2.toSet)
    val mutated = mutatedPart1 ::: mutatedPart2 ::: mutatedPart3
    mutated
  }
  def calculateStatistics(population:Array[EvaluatedSolution]):(Double,Int,Int) = {
    val makespans = population.map(_.value)
    val mean = makespans.sum.asInstanceOf[Double] / population.size
    val median = makespans.apply(population.size / 2 + 1)
    val minimum = makespans.reduceLeft(_ min _)
    (mean,median,minimum)
  }
  //popSize:30, crossRate:1.0, mutRate: 0.8, mutDecreaseFactor: 0.99, mutResetThreshold: 0.95
  def evaluate(p: Problem, popSize: Int, crossRate: Double, mutRate: Double, mutDecreaseFactor: Double, mutResetThreshold: Double): EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
    var population = initNEHplusRandom(p, popSize, initEndTimesMatrix)
    population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
    //calculate population statistics
    var triple = calculateStatistics(population)
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
      if (randomNo < crossRate) {                          //crossover
        //select parent1 using fitness_rank distribution
        val parent1 = population(medianIndex+Random.nextInt(popSize-medianIndex))
        //select parent2 using uniform distribution
        val parent2 = population(Random.nextInt(popSize))
        val children = crossoverC1(parent1.solution.toList, parent2.solution.toList)
        child1 = p.evaluatePartialSolution(children._1.toArray, p.jobTimesMatrix, initEndTimesMatrix)
        child2 = p.evaluatePartialSolution(children._2.toArray, p.jobTimesMatrix, initEndTimesMatrix)
        /*println("parent1 "+parent1)
        println("parent2 "+parent2)
        println("child1 "+child1)
        println("child2 "+child2)*/
      }
      if (randomNo < mutRate) {                            //mutation
        child1 = p.evaluatePartialSolution(mutationSWAP(child1.solution.toList).toArray, p.jobTimesMatrix, initEndTimesMatrix)
        child2 = p.evaluatePartialSolution(mutationSWAP(child2.solution.toList).toArray, p.jobTimesMatrix, initEndTimesMatrix)
      }
      //delete sequence from unfit members, whose makespan value is below the median
      val index1 = Random.nextInt(medianIndex)
      val index2 = Random.nextInt(medianIndex)
      //insert new members into population (at the same time deleting old members)
      population(index1) = child1
      population(index2) = child2
      //update statistics
      population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
      triple = calculateStatistics(population)
      mean = triple._1
      median = triple._2
      minimum = triple._3
      /*println("median "+median)
      println("minimum "+minimum)*/
      //update mutation rate
      mutationRate = mutDecreaseFactor * mutationRate
      //threshold for mutation rate reseting
      if (minimum / mean > mutResetThreshold) mutationRate = mutRate
    } //endwhile
    population = population.sortBy[Int](_.value)(Ordering.Int.reverse)
    population(popSize-1)//return best solution
  }
}