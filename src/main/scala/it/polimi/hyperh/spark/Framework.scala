package it.polimi.hyperh.spark

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.Algorithm
import util.Timeout
import it.polimi.hyperh.solution.DummyEvaluatedSolution

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
    
    val dataset = DistributedDataset(numOfNodes, algorithms, seeds, iterationTimeLimit)
    val rdd = sc.parallelize(dataset).cache
    val solution = hyperLoop(problem, rdd, iterations)
    println(solution)
  }
  def hyperMap(problem: Problem, d: DistributedDatum): EvaluatedSolution = {
    d.algorithm.evaluate(problem, d.seed, d.iterationTimeLimit)
  }
  
  def hyperReduce(sol1: EvaluatedSolution, sol2: EvaluatedSolution): EvaluatedSolution = {
    List(sol1, sol2).min
  }
  def hyperLoop(problem: Problem, rdd: RDD[DistributedDatum], maxIter: Int):EvaluatedSolution = {

    def applyIteration(problem: Problem, rdd: RDD[DistributedDatum]):EvaluatedSolution = {
      rdd.map(datum => hyperMap(problem, datum)).reduce(hyperReduce(_,_))
    }
    def iterloop(rdd: RDD[DistributedDatum], iter:Int, bestSolution: EvaluatedSolution):EvaluatedSolution = 
      if(iter <= maxIter) {
        val newIter = iter+1
        val bestIterSolution = applyIteration(problem, rdd)
        val newBest = List(bestIterSolution, bestSolution).min
        //modify seed
        val updatedRDD = rdd.map(d => DistributedDatum(d.algorithm, Some(newBest), d.iterationTimeLimit))
        iterloop(updatedRDD, newIter, newBest)
      }
      else {
        bestSolution
      }
    iterloop(rdd, 1, DummyEvaluatedSolution(problem))
  }
}