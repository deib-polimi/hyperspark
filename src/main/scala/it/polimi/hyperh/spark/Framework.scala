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
  private var sparkContext: Option[SparkContext] = None
  private var conf: Option[FrameworkConf] = None
  def setConf(fc: FrameworkConf) = { conf = Some(fc) }
  def getConf(): FrameworkConf = conf.getOrElse(throw new RuntimeException("FrameworkConf not set"))
  private def getSparkContext(): SparkContext = sparkContext.getOrElse(throw new RuntimeException("SparkContext error"))
  private var notStarted: Boolean = true
  private var handler: MapReduceHandler = new MapReduceHandler()
  
  def run(conf: FrameworkConf): EvaluatedSolution = {
    //problem specific settings
    val problem = conf.getProblem()
    val algorithms = conf.getAlgorithms()
    val numOfTasks = algorithms.size
    val seeds = conf.getSeeds()
    val iterationTimeLimit = conf.getIterationTimeLimit()
    val iterations = conf.getNumberOfIterations()
    val totalTimeLimit = iterationTimeLimit * iterations
    val dataset = DistributedDataset(numOfTasks, algorithms, seeds, iterationTimeLimit)
    //spark specific settings
    val sparkConf = new SparkConf().setAll(conf.getProperties())
    if(notStarted){//allow only one instance of SparkContext to run
      sparkContext = Some(new SparkContext(sparkConf))
      notStarted = false
    }
    val sc = getSparkContext()
    val rdd = sc.parallelize(dataset).cache
    handler = conf.getMapReduceHandler()
    val solution = hyperLoop(problem, rdd, iterations, 1)
    solution
  }
  def multipleRuns(conf: FrameworkConf, runs: Int): Array[EvaluatedSolution] = {
    //problem specific settings
    val problem = conf.getProblem()
    val algorithms = conf.getAlgorithms()
    val numOfTasks = algorithms.size
    val seeds = conf.getSeeds()
    val iterationTimeLimit = conf.getIterationTimeLimit()
    val iterations = conf.getNumberOfIterations()
    val totalTimeLimit = iterationTimeLimit * iterations
    val dataset = DistributedDataset(numOfTasks, algorithms, seeds, iterationTimeLimit)
    //spark specific settings
    val sparkConf = new SparkConf().setAll(conf.getProperties())
    if(notStarted){//allow only one instance of SparkContext to run
      sparkContext = Some(new SparkContext(sparkConf))
      notStarted = false
    }
    val sc = getSparkContext()
    val rdd = sc.parallelize(dataset).cache
    handler = conf.getMapReduceHandler()
    var solutions: Array[EvaluatedSolution] = Array()
     for(i <- 1 to runs) {
       val solution = hyperLoop(problem, rdd, iterations, i)
       solutions :+= solution
     }
    solutions
  }
  def stop() = {
    val sc = getSparkContext()
    sc.stop()
    sparkContext = None
  }
  
  def hyperLoop(problem: Problem, rdd: RDD[DistributedDatum], maxIter: Int, runNo: Int):EvaluatedSolution = {

    def applyIteration(problem: Problem, rdd: RDD[DistributedDatum]):EvaluatedSolution = {
      rdd.map(datum => handler.hyperMap(problem, datum, runNo)).reduce(handler.hyperReduce(_,_))
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