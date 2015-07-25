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
object NEHAlgorithm {
  def apply(p: Problem): EvaluatedSolution = {
    evaluate(p)
  }
  def evaluate(p:Problem):EvaluatedSolution = {
      val initEndTimesMatrix = p.jobsInitialTimes()  
      val pairs = p.createJobValuePairs(p.jobs, p.extractEndTimes(initEndTimesMatrix))
      //STEP 1: sort jobs in decreasing order, STEP 2.1.take best two,
      val twoJobs = p.getWorstNJobs(pairs,2).map(x => x._1).toList //> twoJobs  : List[Int] = List(1, 2)
      val remainingJobs = p.jobs.filterNot(twoJobs.toSet).toList //> remainingJobs  : List[Int] = List(3, 4, 5)
      
      def loop(listOfTwo: List[Int], remainingJobs:List[Int]):EvaluatedSolution = {
        //STEP 2.2 get best permutation of two jobs
        var bestPermutation = PermutationUtility.getBestPermutation(PermutationUtility.generatePermutations(listOfTwo),p,initEndTimesMatrix)
        //STEP 3 of NEH algorithm
        //from 0 until numOfRemainingJobs (in NEH this is marked as for k=3 to numOfJobs)
        for(i <- 0 until remainingJobs.size) {
          val genPermutations = PermutationUtility.generateInserts(bestPermutation.solution.toList,remainingJobs(i))
          bestPermutation = PermutationUtility.getBestPermutation(genPermutations,p,initEndTimesMatrix)
          //println(bestPermutation)
        }
        bestPermutation
      }
      val finalSolution = loop(twoJobs, remainingJobs)   //> mySolution  : solution.EvaluatedSolution = EvaluatedSolution(58,[I@2353b3e6| )
      finalSolution
    }
}