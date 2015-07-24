package it.polimi.hyperh.problem
import scala.util.Random
import Array._
import scala.io.Source
import it.polimi.hyperh._
import it.polimi.hyperh.Types._
import util.ProblemParser
import solution.Solution
import solution.EvaluatedSolution
import types.PermutationUtility
/**
 * @author Nemanja
 */
/*class IGAlgorithm() extends Serializable {

}*/
//Problem Factory
object IGAlgorithm {

  def apply(p:Problem,d:Int,T:Double): EvaluatedSolution = {
    evaluate(p,d,T)
  }
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
        //println(bestPermutation)
    }
    bestPermutation
  }                                               //> construction: (left: List[Int], removed: List[Int])Array[Int]
  //construction(List(1, 2, 4),List(3, 5))          //> res0: Array[Int] = Array(5, 1, 2, 3, 4)
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
        val localSolution = PermutationUtility.getBestPermutation(PermutationUtility.generateInserts(tmp.toList,job),p,initEndTimesMatrix)
        if(localSolution.value < bestSolution.value) {
          bestSolution = localSolution
          improve=true
        }
      }
    }
    bestSolution
  }       
  
  def evaluate(p:Problem,d:Int,T:Double):EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
    var currentSolution = NEHAlgorithm.evaluate(p)
    currentSolution = localSearch(currentSolution.solution,p,initEndTimesMatrix)//improve it by local search
    var bestSolution = currentSolution
     //termination is n*(m/2)*60 milliseconds
    var timeLimit = p.numOfMachines*(p.numOfJobs/2.0)*60  //in Millis
    def setTimeout(limit: Double) = {//introduce thread time here
      val expireTime = System.currentTimeMillis() + limit
      expireTime
    }
    val expireTimeMillis = setTimeout(timeLimit)
    def notTimeout():Boolean = {
      if(System.currentTimeMillis() > expireTimeMillis)
        false
      else true
    }
    while(notTimeout()) {
      val pair = IGAlgorithm.destruction(currentSolution.solution, d)
      val bestPermutation = construction(pair._1, pair._2,p,initEndTimesMatrix)
      bestSolution = p.evaluatePartialSolution(bestPermutation,p.jobTimesMatrix,initEndTimesMatrix)
      val improvedSolution = localSearch(bestPermutation,p,initEndTimesMatrix)
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