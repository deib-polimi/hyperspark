package pfsp.algorithms

import Array._
import it.polimi.hyperh._
import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.Algorithm
import pfsp.util.PermutationUtility
import pfsp.solution.NaivePfsEvaluatedSolution
import pfsp.problem.PfsProblem
import pfsp.solution.PfsSolution
import pfsp.solution.PfsEvaluatedSolution
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired


/**
 * @author Nemanja
 */

//Problem Factory
class IGAlgorithm(val d:Int,val T:Double, seedOption: Option[PfsSolution]) extends Algorithm {
  /**
   * A secondary constructor.
   */
  def this() {
    //d: 4, T: 0.4
    this(4, 0.4, None)
  }
  def this(seedOption: Option[PfsSolution]) {
    this(4, 0.4, seedOption)
  }
  seed = seedOption 
  
  def initNEHSolution(p: PfsProblem): PfsEvaluatedSolution = {
    val nehAlgorithm = new NEHAlgorithm()
    nehAlgorithm.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
  }
  def initialSolution(p: PfsProblem): PfsEvaluatedSolution = {
    seed match {
      case Some(seed) => seed.evaluate(p).asInstanceOf[PfsEvaluatedSolution]
      case None => initNEHSolution(p)
    }
  }
  override def evaluate(problem:Problem):EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val timeLimit = p.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    evaluate(p, stopCond)
  }  
  override def evaluate(problem:Problem, stopCond: StoppingCondition):EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()
    val dummySol = NaivePfsEvaluatedSolution(p)
    def loop(currentSol: PfsEvaluatedSolution, bestSol: PfsEvaluatedSolution, iter: Int): PfsEvaluatedSolution = {
      if(stop.isNotSatisfied()) {
        var currentSolution = currentSol
        var bestSolution = bestSol
		//initialisation phase
        if(iter == 1){
          currentSolution = initialSolution(p)
          currentSolution = localSearch(currentSolution.permutation,p)//improve it by local search
          bestSolution = currentSolution
        }
		//in each phase do
        val pair = destruction(currentSolution.permutation, d)
        val bestPermutation = construction(pair._1, pair._2,p)
        bestSolution = p.evaluate(PfsSolution(bestPermutation)).asInstanceOf[PfsEvaluatedSolution]
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
          if(random.nextDouble() <= Math.exp(-(improvedSolution.value-currentSolution.value)/calculateConstant(T)))
            currentSolution = improvedSolution
        }
        loop(currentSolution, bestSolution, iter+1)
      }
      else bestSol
    }
    loop(dummySol, dummySol, 1)
  }
  override def evaluate(p:Problem, seedSol: Option[Solution], stopCond: StoppingCondition):EvaluatedSolution = {
    seed = seedSol
    evaluate(p, stopCond)
  }
  //removes d jobs from permutation list, and produces two lists (left,removed)
  def destruction(permutation: Array[Int],d: Int): (List[Int],List[Int]) = {
    var tmp=permutation.toBuffer//used buffer because of performance, random access and changable size
    var removed=List[Int]()
    for(i <- 0 until d) {
      val size = tmp.size
      val removeInd = random.nextInt(size);//returns int between 0 (inclusive) and the specified value (exclusive)
      val el = tmp.remove(removeInd)
      removed = removed ::: List(el)
    }
    val left = tmp.toList//convert buffer to list
    (left, removed)
  }
  def construction(left:List[Int], removed: List[Int],p:PfsProblem) = { 
    var bestPermutation = left.toArray
    //STEP 3 of NEH algorithm
    //from 0 until numOfRemainingJobs (in NEH this is marked as for k=3 to numOfJobs)
    for(i <- 0 until removed.size) {
      val genPermutations = PermutationUtility.generateInserts(bestPermutation.toList,removed(i))
        bestPermutation = PermutationUtility.getBestPermutation(genPermutations,p).permutation
    }
    bestPermutation
  }
  //Iterative improvement based on insertion
  def localSearch(permutation: Array[Int],p:PfsProblem):PfsEvaluatedSolution = {
    var bestSolution = p.evaluate(PfsSolution(permutation)).asInstanceOf[PfsEvaluatedSolution]
    var improve = true
    while(improve == true) {
      improve = false
      val indexes = (0 until permutation.size).toList
      var removalOrder = random.shuffle(indexes)
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
