package it.polimi.hyperh.algorithms

import scala.util.Random
import Array._
import it.polimi.hyperh._
import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.solution.EvaluatedSolution
import util.PermutationUtility
import it.polimi.hyperh.problem.Problem
import util.Timeout
import it.polimi.hyperh.solution.Solution
/**
 * @author Nemanja
 */

//Problem Factory
class IGAlgorithm(val d:Int,val T:Double) extends Algorithm {
  /**
   * A secondary constructor.
   */
  def this() {
    //d: 2, T: 0.2
    this(2, 0.2)
  }
  
  override def evaluate(p:Problem):EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
    val nehAlgorithm = new NEHAlgorithm()
    var currentSolution = nehAlgorithm.evaluate(p)
    currentSolution = IGAlgorithm.localSearch(currentSolution.solution,p,initEndTimesMatrix)//improve it by local search
    var bestSolution = currentSolution
    
    val timeLimit = p.numOfMachines*(p.numOfJobs/2.0)*60//termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    while(Timeout.notTimeout(expireTimeMillis)) {
      val pair = IGAlgorithm.destruction(currentSolution.solution, d)
      val bestPermutation = IGAlgorithm.construction(pair._1, pair._2,p,initEndTimesMatrix)
      bestSolution = p.evaluatePartialSolution(bestPermutation,p.jobTimesMatrix,initEndTimesMatrix)
      val improvedSolution = IGAlgorithm.localSearch(bestPermutation,p,initEndTimesMatrix)
      //pi - currentSolution,piPrime - bestSolution, piSecond - improvedSolution
      if(improvedSolution.value < currentSolution.value){//acceptance criterion
        currentSolution = improvedSolution
        if(currentSolution.value < bestSolution.value)//check if new best permutation
          bestSolution = currentSolution
      } else {
        def calculateConstant(T: Double):Double = {
          def sumJobTimes = p.jobTimesMatrix.map(x => x.sum).sum
          val constant = T * (sumJobTimes / (p.numOfMachines*p.numOfJobs*10))
          constant
        }
        if(Random.nextDouble() <= Math.exp(-(improvedSolution.value-currentSolution.value)/calculateConstant(T)))
          currentSolution = improvedSolution
      }
    }
    bestSolution
  }  
}
object IGAlgorithm {
  
  //removes d jobs from permutation list, and produces two lists (left,removed)
  def destruction(permutation: Array[Int],d: Int): (List[Int],List[Int]) = {
    var tmp=permutation.toBuffer//used buffer because of performance, random access and changable size
    var removed=List[Int]()
    for(i <- 0 until d) {
      val size = tmp.size
      val removeInd = Random.nextInt(size);//returns int between 0 (inclusive) and the specified value (exclusive)
      val el = tmp.remove(removeInd)
      removed = removed ::: List(el)
    }
    val left = tmp.toList//convert buffer to list
    (left, removed)
  }
  def construction(left:List[Int], removed: List[Int],p:Problem,initEndTimesMatrix:Array[Array[Int]]) = { 
    var bestPermutation = left.toArray
    //STEP 3 of NEH algorithm
    //from 0 until numOfRemainingJobs (in NEH this is marked as for k=3 to numOfJobs)
    for(i <- 0 until removed.size) {
      val genPermutations = PermutationUtility.generateInserts(bestPermutation.toList,removed(i))
        bestPermutation = PermutationUtility.getBestPermutation(genPermutations,p,initEndTimesMatrix).solution
    }
    bestPermutation
  }
  //Iterative improvement based on insertion
  def localSearch(permutation: Array[Int],p:Problem, initEndTimesMatrix:Array[Array[Int]]):EvaluatedSolution = {
    var bestSolution = p.evaluatePartialSolution(permutation,p.jobTimesMatrix,initEndTimesMatrix)
    var improve = true
    while(improve == true) {
      improve = false
      val indexes = (0 until permutation.size).toList
      var removalOrder = Random.shuffle(indexes)
      for(i <- 0 until removalOrder.size) {
        var tmp=permutation.toBuffer
        val job = tmp(removalOrder(i))
        tmp.remove(removalOrder(i))
        val insertsList = PermutationUtility.generateInserts(tmp.toList,job)
        val localSolution = PermutationUtility.getBestPermutation(insertsList,p,initEndTimesMatrix)
        if(localSolution.value < bestSolution.value) {
          bestSolution = localSolution
          improve=true
        }
      }
    }
    bestSolution
  }       
  
  
}