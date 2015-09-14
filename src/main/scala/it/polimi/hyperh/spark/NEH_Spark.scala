package it.polimi.hyperh.spark

import Array._
import it.polimi.hyperh._
import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.problem.Problem
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import util.Timeout
import org.apache.spark.SparkConf

/**
 * @author Nemanja
 */
/*class NEHAlgorithm() extends Serializable {
  
}*/
object NEH_Spark {

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("HyperH").setMaster("local[4]")
    val sc = new SparkContext(conf)

    def hyperMap(problem: Problem, solution: List[Int], position: Int, value: Int, iter: Int): EvaluatedSolution = {
      def insertAt(value: Int, position: Int, list: List[Int]) = {
        if (position > list.size)
          list ::: List(value)
        else
          (list take position - 1) ::: List(value) ::: (list drop position - 1)
      }
      problem.evaluatePartialSolution(insertAt(value, position, solution))
    }

    def hyperReduce(sol1: EvaluatedSolution, sol2: EvaluatedSolution): EvaluatedSolution = {
      if (sol1.value < sol2.value)
        sol1
      else
        sol2
    }

    def loop(p: Problem, timeLimit: Double): EvaluatedSolution = {
      
      val expireTimeMillis =  Timeout.setTimeout(timeLimit)
      
      def initializeSolution(problem: Problem): EvaluatedSolution = {
        val pairs = p.createJobValuePairs(p.jobs, p.extractEndTimes(p.initEndTimesMatrix))
        //STEP 1: sort jobs in decreasing order, STEP 2.1.take best two,
        val twoJobs = p.getWorstNJobs(pairs, 2).map(x => x._1).toList
        //STEP 2.2 get best permutation of two jobs
        val firstSol = p.evaluatePartialSolution(twoJobs)
        val secondSol = p.evaluatePartialSolution(twoJobs.reverse)
        if (firstSol.value < secondSol.value)
          firstSol
        else
          secondSol

      }
      def applyIteration(p: Problem, solutionList: List[Int], positionsRDD: RDD[Int], value: Int, iter: Int): EvaluatedSolution = {
        positionsRDD.map(position => hyperMap(p, solutionList, position, value, iter)).reduce(hyperReduce)
      }

      def iterloop(remainingJobs: List[Int], iter: Int, evSolution: EvaluatedSolution): EvaluatedSolution = {
        if (Timeout.notTimeout(expireTimeMillis+6000)) {
          if (iter == 1) {
            //STEP 1: sort jobs in decreasing order, STEP 2.1.take best two,
            val pairs = p.createJobValuePairs(p.jobs, p.extractEndTimes(p.initEndTimesMatrix))
            val sortedList = p.sortJobsDecreasing(pairs).map(x => x._1).toList
            val twoJobs = sortedList.take(2)
            //STEP 2.2 get best permutation of two jobs
            val firstSol = p.evaluatePartialSolution(twoJobs)
            val secondSol = p.evaluatePartialSolution(twoJobs.reverse)
            var evSol = secondSol
            if (firstSol.value < secondSol.value)
              evSol = firstSol
            val remJobs = sortedList.filterNot(twoJobs.toSet).toList
            iterloop(remJobs, iter + 1, evSol)
          }
          else {
            if (remainingJobs.size > 0 && remainingJobs.size <= p.numOfJobs-2 ) {
              val newIter = iter+1
              val solutionList = evSolution.solution.toList
              val positionsRDD = sc.parallelize(1 to solutionList.size+1, solutionList.size+1).cache
              val nextSolution = applyIteration(p, solutionList, positionsRDD, remainingJobs.head, newIter)
              iterloop(remainingJobs.tail, newIter, nextSolution)
            } 
            else {
              evSolution
            }
          }
        } 
        else evSolution
      }
      iterloop(p.jobs.toList, 1, new EvaluatedSolution(999999999, p.jobs))
    }

    //read the problem
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val p = Problem(path + "inst_ta002").getOrElse(throw new RuntimeException("ParserError"))

    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds

    val start = System.currentTimeMillis()
    val solution = loop(p, timeLimit)
    println(solution)
    val end = System.currentTimeMillis() 
    println("duration "+(end - start))

  }
}
