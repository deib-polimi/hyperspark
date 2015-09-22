package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import scala.util.Random
import it.polimi.hyperh.search.NeighbourhoodSearch
import util.Timeout
import it.polimi.hyperh.solution.Solution
import util.RNG

/**
 * @author Nemanja
 */
class MMMASAlgorithm(p: Problem, t0: Double, cand: Int, seedOption: Option[Solution], rng: RNG) 
extends MMASAlgorithm(p,t0,cand,seedOption, rng) {
  /**
   * A secondary constructor.
   */
  def this(p: Problem, seedOption: Option[Solution], rng: RNG) {
    this(p, 0.2, 5, seedOption, rng)//default values
  }
  def this(p: Problem, seedOption: Option[Solution]) {
    this(p, 0.2, 5, seedOption, RNG())//default values
  }
  def this(p: Problem, rng: RNG) {
    this(p, 0.2, 5, None, rng)//default values
  }
  def this(p: Problem) {
    this(p, 0.2, 5, None, RNG())//default values
  }
  def sumij(iJob: Int, jPos : Int) = {
    var sum = 0.0
    for(q <- 1 to jPos)
      sum = sum + trail(iJob, jPos)
    sum
  }
  override def probability(i: Int, j: Int, scheduled: List[Int], notScheduled: List[Int]): Double = {
    if(scheduled.contains(j))
      0
    else{
      def sumTrails(list: List[Int]): Double = {
        def sum(list: List[Int], acc: Double): Double = {
          list match {
            case List() => acc
            case _ => sum(list.tail, acc + trail(list.head, j))
          }
        }
        sum(list, 0)
      }
      val pij = sumij(i, j) / sumTrails(notScheduled)//this part is changed
      pij
    }
  }
  override def constructAntSolution(bestSolution: EvaluatedSolution): EvaluatedSolution = {  
    var scheduled: List[Int] = List()
    var jPos = 1
    var notScheduled = (1 to p.numOfJobs).toList
    var candidates: List[Int] = List()
    
    while(jPos <= p.numOfJobs) {
      var nextJob = -1
      var u = rng.nextDouble()
      if(u <= p0) {
        candidates = bestSolution.solution.toList.filterNot(job => scheduled.contains(job)).take(cand)
        var max = 0.0
        for(i <- 0 until candidates.size) {
          val sij = sumij(candidates(i), jPos)
          if(sij > max) {
            max = sij
            nextJob = candidates(i)
          }
        }
      }
      else {
        candidates = bestSolution.solution.toList.filterNot(job => scheduled.contains(job)).take(cand)
        nextJob = getNextJob(scheduled, candidates, jPos)
      }     
      scheduled = scheduled ::: List(nextJob)
      jPos = jPos + 1
    }
    p.evaluatePartialSolution(scheduled)
  }
  //job-index-based local search
  override def localSearch(completeSolution: EvaluatedSolution, expireTimeMillis: Double): EvaluatedSolution = {
    var bestSolution = completeSolution
    for(runs <- 1 to 3) {
      val seed = bestSolution.solution
      val seedList = seed.toList
      var i = 1
      while(i <= p.numOfJobs && Timeout.notTimeout(expireTimeMillis)){
        var j = 1
        while(j <= p.numOfJobs && Timeout.notTimeout(expireTimeMillis)) {
          if(seed(j-1) != i) {
            val remInd = seed.indexWhere( _ == i)
            val insInd = j-1
            val neighbourSol = NeighbourhoodSearch(rng).INSdefineMove(seedList, remInd, insInd)
            val evNeighbourSol = p.evaluatePartialSolution(neighbourSol)
            if(evNeighbourSol.value < bestSolution.value)
              bestSolution = evNeighbourSol
          }
          j = j + 1
        }//while j
        i = i + 1
      }//while i
    }//runs
    bestSolution
  }
  //overide solution passed to updatePheromones: ant best instead of global best solution
  override def updatePheromones(antSolution: EvaluatedSolution, bestSolution: EvaluatedSolution) = {
    updateTmax(bestSolution)
    updateTmin
    val usedSolution = antSolution
    def deposit(iJob: Int,jPos: Int): Double = {
      if(usedSolution.solution(jPos-1) == iJob)
        1.0/usedSolution.value  //using antSolution instead of bestSolution
      else
        0.0
    }
    for(i <- 1 to p.numOfJobs)
      for(j <- 1 to p.numOfJobs) {
        val newTij = persistenceRate * trail(i,j) + deposit(i,j)
        setT(i, j, newTij)
      }
  }
  
  
}