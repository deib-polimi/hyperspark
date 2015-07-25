package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random

/**
 * @author Nemanja
 */
object GAAlgorithm {
  def apply(p:Problem): EvaluatedSolution = {
    evaluate(p)
  }
  def initRandom(p:Problem) = {
    p.jobs
  }
  def initNEH(p:Problem) = {
    p.jobs
  }
  def initNEHplusRandom(p:Problem) = {
    p.jobs
  }
  def crossoverLOX(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
    val size = parent1.size
    val firstPoint = Random.nextInt(size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(size - firstPoint)//[firstPoint+1,n]
    val p1Remove = parent2.drop(firstPoint).take(secondPoint-firstPoint)
    val p2Remove = parent1.drop(firstPoint).take(secondPoint-firstPoint)
    val p1Filtered = parent1.filterNot(p1Remove.toSet)
    val p2Filtered = parent2.filterNot(p2Remove.toSet)
    val p1Reconstructed = p1Filtered.take(firstPoint):::p1Remove:::p1Filtered.drop(firstPoint)
    val p2Reconstructed = p2Filtered.take(firstPoint):::p2Remove:::p2Filtered.drop(firstPoint)
    (p1Reconstructed, p2Reconstructed)
  }
  def crossoverPMX(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
    val size = parent1.size
    val firstPoint = 2//Random.nextInt(size - 1)//[0,n-2]
    val secondPoint = 6//firstPoint + 1 + Random.nextInt(size - firstPoint)//[firstPoint+1,n]
    val mapPart1 = parent1.drop(firstPoint).take(secondPoint-firstPoint)
    val mapPart2 = parent2.drop(firstPoint).take(secondPoint-firstPoint)
    val mappings = ((mapPart1 zip mapPart2) ::: (mapPart2 zip mapPart1)).toSet
    
    def applyMapping(parent:List[Int], mappings: Set[(Int,Int)],result:List[Int]):List[Int] = parent match {
      case List()  => result
      case x :: xs => mappings.filter(y => y._1 == x).toList match {
        case List() => applyMapping(xs, mappings, result ::: List(x))
        case List(m) => applyMapping(xs, mappings.filter(y => y._1 != m._1 && y._2 != m._2), result ::: List(m._2))
      }
    }
    val child1 = applyMapping(parent1,mappings, List())
    val child2 = applyMapping(parent2,mappings, List())
    (child1,child2)
  }
  def evaluate(p:Problem):EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
    var currentSolution = NEHAlgorithm.evaluate(p)
    var bestSolution = currentSolution
    bestSolution
  }
}