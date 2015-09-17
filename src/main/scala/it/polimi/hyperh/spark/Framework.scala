package it.polimi.hyperh.spark

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.Algorithm
import util.Timeout

/**
 * @author Nemanja
 */
object Framework {
  def run(conf: FrameworkConf) = {
    val sparkConf = new SparkConf().setAppName("HyperH").setMaster(conf.getSparkMaster())
    val sc = new SparkContext(sparkConf)
    val numOfNodes = conf.getNumOfNodes()
    val problem = conf.getProblem()
    val algorithms = conf.getAlgorithms()
    val seeds = conf.getSeeds()
    val iterationTimeLimit = conf.getIterationTimeLimit()
    val iterations = conf.getNumberOfIterations()
    val totalTimeLimit = iterationTimeLimit * iterations
    
    val tupple3Array = algorithms zip seeds zip Array.fill(numOfNodes)(iterationTimeLimit)
    val rdd = sc.parallelize(tupple3Array).cache
    val solution = hyperLoop(problem, rdd, iterations)
    println(solution)
    
    //rdd[(Algorithm, seed, timeLimit)]
    
  }
  def hyperMap(problem: Problem, alg: Algorithm, seed: Option[Solution],timeLimit: Double, iter: Int): EvaluatedSolution = {
    alg.evaluate(problem, seed, timeLimit)
  }
  
  def hyperReduce(sol1: EvaluatedSolution, sol2: EvaluatedSolution): EvaluatedSolution = {
    if(sol1.value < sol2.value)
      new EvaluatedSolution(sol1.value, sol1.solution)
    else
      new EvaluatedSolution(sol2.value, sol2.solution)
  }
  def hyperLoop(problem: Problem, rdd: RDD[(Algorithm, Option[Solution], Double)], maxIter: Int):EvaluatedSolution = {

    def applyIteration(problem: Problem, solution: List[Int], rdd: RDD[(Algorithm, Option[Solution], Double)],iter:Int):EvaluatedSolution = {
      rdd.map(t=>hyperMap(problem, t._1, t._2, t._3, iter)).reduce(hyperReduce(_,_))
    }
    def iterloop(rdd: RDD[(Algorithm, Option[Solution], Double)], iter:Int, bestSolution: EvaluatedSolution):EvaluatedSolution = 
      //def convertRdd[U >: T : Solution]
      if(iter <= maxIter) {
        val newIter = iter+1
        val bestSolutionList = bestSolution.solution.toList
        val bestIterSolution = applyIteration(problem, bestSolutionList , rdd , newIter)
        var newBest = bestSolution
        if(bestIterSolution.value < bestSolution.value)
          newBest = bestIterSolution
        //modify seed
        val updatedRDD = rdd.map(t => (t._1, Some(newBest), t._3))
        //updatedRDD.asInstanceOf[RDD[(Algorithm, Option[Solution], Double)]]
        iterloop(updatedRDD.asInstanceOf[RDD[(Algorithm, Option[Solution], Double)]], newIter, newBest)
      }
      else {
        bestSolution
      }
    iterloop(rdd, 1, new EvaluatedSolution(999999999, problem.jobs))
  }
}