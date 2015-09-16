package it.polimi.hyperh.algorithms

import Array._
import it.polimi.hyperh._
import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.solution.EvaluatedSolution
import util.PermutationUtility
import it.polimi.hyperh.problem.Problem

/**
 * @author Nemanja
 */
/*class NEHAlgorithm() extends Serializable {
  
}*/

//Problem Factory
class NEHAlgorithm() extends Algorithm {
  override def evaluate(p: Problem): EvaluatedSolution = {
    val pairs = p.createJobValuePairs(p.jobs, p.extractEndTimes(p.initEndTimesMatrix))
    //STEP 1: sort jobs in decreasing order, STEP 2.1.take best two,
    val sortedList = p.sortJobsDecreasing(pairs).map(x => x._1).toList
    val twoJobs = sortedList.take(2) //> twoJobs  : List[Int] = List(1, 2)
    val remainingJobs = sortedList.filterNot(twoJobs.toSet).toList //> remainingJobs  : List[Int] = List(3, 4, 5)

    def loop(partialSolution: List[Int], remainingJobs: List[Int]): EvaluatedSolution = {
      //STEP 2.2 get best permutation of two jobs
      var bestPermutation = PermutationUtility.getBestPermutation(PermutationUtility.generatePermutations(partialSolution), p)
      //STEP 3 of NEH algorithm
      //from 0 until numOfRemainingJobs (in NEH this is marked as for k=3 to numOfJobs)
      for (i <- 0 until remainingJobs.size) {
        val genPermutations = PermutationUtility.generateInserts(bestPermutation.solution.toList, remainingJobs(i))
        bestPermutation = PermutationUtility.getBestPermutation(genPermutations, p)
        //println(bestPermutation)
      }
      bestPermutation
    }
    val finalSolution = loop(twoJobs, remainingJobs) //> mySolution  : solution.EvaluatedSolution = EvaluatedSolution(58,[I@2353b3e6| )
    finalSolution
  }
  //useless to have time limit on NEH, because it will not construct complete solution
  //by the way, NEH is pretty fast comparing to other algorithms, so it will generate
  //complete solution faster than other algorithms
  override def evaluate(p: Problem, timeLimit: Double): EvaluatedSolution = {
    evaluate(p)
  }
}

