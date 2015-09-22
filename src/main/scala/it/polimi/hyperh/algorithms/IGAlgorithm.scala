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
import util.RNG
/**
 * @author Nemanja
 */

//Problem Factory
class IGAlgorithm(val d:Int,val T:Double, seedOption: Option[Solution], rng: RNG) extends Algorithm {
  /**
   * A secondary constructor.
   */
  def this() {
    //d: 2, T: 0.2
    this(2, 0.2, None, RNG())
  }
  def this(seedOption: Option[Solution]) {
    this(2, 0.2, seedOption, RNG())
  }
  def this(rng: RNG) {
    this(2, 0.2, None, rng)
  }
  private var seed = seedOption 
  
  def initNEHSolution(p: Problem) = {
    val nehAlgorithm = new NEHAlgorithm()
    nehAlgorithm.evaluate(p)
  }
  def initialSolution(p: Problem): EvaluatedSolution = {
    seed match {
      case Some(seed) => seed.evaluate(p)
      case None => initNEHSolution(p)
    }
  }
  override def evaluate(p:Problem):EvaluatedSolution = {
    val timeLimit = p.numOfMachines*(p.numOfJobs/2.0)*60//termination is n*(m/2)*60 milliseconds
    evaluate(p, timeLimit)
  }  
  override def evaluate(p:Problem, timeLimit: Double):EvaluatedSolution = {
    val dummySol = new EvaluatedSolution(999999999, p.jobs)//dummy initialization
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    def loop(currentSol: EvaluatedSolution, bestSol: EvaluatedSolution, iter: Int): EvaluatedSolution = {
      if(Timeout.notTimeout(expireTimeMillis)) {
        var currentSolution = currentSol
        var bestSolution = bestSol
        if(iter == 1){
          currentSolution = initialSolution(p)
          currentSolution = localSearch(currentSolution.solution,p)//improve it by local search
          bestSolution = currentSolution
        }
        val pair = destruction(currentSolution.solution, d)
        val bestPermutation = construction(pair._1, pair._2,p)
        bestSolution = p.evaluatePartialSolution(bestPermutation)
        val improvedSolution = localSearch(bestPermutation,p)
        //pi - currentSolution,piPrime - bestSolution, piSecond - improvedSolution
        if(improvedSolution.value < currentSolution.value){//acceptance criterion
          currentSolution = improvedSolution
          if(currentSolution.value < bestSolution.value)//check if new best permutation
            bestSolution = currentSolution
        } 
        else {
          def calculateConstant(T: Double):Double = {
            def sumJobTimes = p.sumJobTimesMatrix()
            val constant = T * (sumJobTimes / (p.numOfMachines*p.numOfJobs*10))
            constant
          }
          if(rng.nextDouble() <= Math.exp(-(improvedSolution.value-currentSolution.value)/calculateConstant(T)))
            currentSolution = improvedSolution
        }
        loop(currentSolution, bestSolution, iter+1)
      }
      else bestSol
    }
    loop(dummySol, dummySol, 1)
  }
  override def evaluate(p:Problem, seedSol: Option[Solution], timeLimit: Double):EvaluatedSolution = {
    seed = seedSol
    evaluate(p, timeLimit)
  }
  //removes d jobs from permutation list, and produces two lists (left,removed)
  def destruction(permutation: Array[Int],d: Int): (List[Int],List[Int]) = {
    var tmp=permutation.toBuffer//used buffer because of performance, random access and changable size
    var removed=List[Int]()
    for(i <- 0 until d) {
      val size = tmp.size
      val removeInd = rng.nextInt(size);//returns int between 0 (inclusive) and the specified value (exclusive)
      val el = tmp.remove(removeInd)
      removed = removed ::: List(el)
    }
    val left = tmp.toList//convert buffer to list
    (left, removed)
  }
  def construction(left:List[Int], removed: List[Int],p:Problem) = { 
    var bestPermutation = left.toArray
    //STEP 3 of NEH algorithm
    //from 0 until numOfRemainingJobs (in NEH this is marked as for k=3 to numOfJobs)
    for(i <- 0 until removed.size) {
      val genPermutations = PermutationUtility.generateInserts(bestPermutation.toList,removed(i))
        bestPermutation = PermutationUtility.getBestPermutation(genPermutations,p).solution
    }
    bestPermutation
  }
  //Iterative improvement based on insertion
  def localSearch(permutation: Array[Int],p:Problem):EvaluatedSolution = {
    var bestSolution = p.evaluatePartialSolution(permutation)
    var improve = true
    while(improve == true) {
      improve = false
      val indexes = (0 until permutation.size).toList
      var removalOrder = rng.shuffle(indexes)
      for(i <- 0 until removalOrder.size) {
        var tmp=permutation.toBuffer
        val job = tmp(removalOrder(i))
        tmp.remove(removalOrder(i))
        val insertsList = PermutationUtility.generateInserts(tmp.toList,job)
        val localSolution = PermutationUtility.getBestPermutation(insertsList,p)
        if(localSolution.value < bestSolution.value) {
          bestSolution = localSolution
          improve=true
        }
      }
    }
    bestSolution
  }  
}
