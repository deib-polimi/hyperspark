package it.polimi.hyperh.spark

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.Algorithm
import util.RoundRobinIterator

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
  private var mrHandler: MapReduceHandler = new MapReduceHandler()
  private var seedingStrategy: SeedingStrategy = new SameSeeds()
  
  def run(conf: FrameworkConf): EvaluatedSolution = {
    setConf(conf)
    //problem specific settings
    val problem = conf.getProblem()
    val algorithms = conf.getAlgorithms()
    val numOfTasks = algorithms.size
    val seeds = conf.getInitialSeeds()
    val stopCond = conf.getStoppingCondition()
    val iterations = conf.getNumberOfIterations()
    val dataset = DistributedDataset(numOfTasks, algorithms, seeds, stopCond)
    //spark specific settings
    val sparkConf = new SparkConf().setAll(conf.getProperties())
    if(notStarted){//allow only one instance of SparkContext to run
      sparkContext = Some(new SparkContext(sparkConf))
      notStarted = false
    }
    val sc = getSparkContext()
    val rdd = sc.parallelize(dataset, numOfTasks).cache
    mrHandler = conf.getMapReduceHandler()
    seedingStrategy = conf.getSeedingStrategy()
    //run the hyperLoop
    val solution = hyperLoop(problem, rdd, iterations, 1)
    solution
  }
  def multipleRuns(conf: FrameworkConf, runs: Int): Array[EvaluatedSolution] = {
    setConf(conf)
    //problem specific settings
    val problem = conf.getProblem()
    val algorithms = conf.getAlgorithms()
    val numOfTasks = algorithms.size
    val seeds = conf.getInitialSeeds()
    val stopCond = conf.getStoppingCondition()
    val iterations = conf.getNumberOfIterations()//coop. iterations to be performed in one run, default: 1
    val dataset = DistributedDataset(numOfTasks, algorithms, seeds, stopCond)
    //spark specific settings
    val sparkConf = new SparkConf().setAll(conf.getProperties())
    if(notStarted){//allow only one instance of SparkContext to run
      sparkContext = Some(new SparkContext(sparkConf))
      notStarted = false
    }
    val sc = getSparkContext()
    val rdd = sc.parallelize(dataset, numOfTasks).cache
    mrHandler = conf.getMapReduceHandler()
    seedingStrategy = conf.getSeedingStrategy()
    //run the hyperLoop
    var solutions: Array[EvaluatedSolution] = Array()
     for(runNo <- 1 to runs) {
       val solution = hyperLoop(problem, rdd, iterations, runNo)
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
      rdd.map(datum => mrHandler.hyperMap(problem, datum, runNo)).reduce(mrHandler.hyperReduce(_,_))
    }
    var bestSolution: EvaluatedSolution = null
    
    def iterloop(rdd: RDD[DistributedDatum], iteration: Int): EvaluatedSolution = {
      val bestIterSolution = applyIteration(problem, rdd)
      if(iteration == 1)  bestSolution = bestIterSolution
      else bestSolution = List(bestIterSolution, bestSolution).min
      
      if(iteration == maxIter)//if it is last iteration don't update the rdd
        bestSolution          //return best solution found
      else {
        val updatedRDD = updateRDD(rdd, bestSolution)
        iterloop(updatedRDD, iteration+1)
      }
    }
    iterloop(rdd, 1)
  }
  def updateRDD(rdd: RDD[DistributedDatum], seed: EvaluatedSolution): RDD[DistributedDatum] = {
    val numOfTasks = getConf().getAlgorithms().size
    val seeds = seedingStrategy.divide(Some(seed), numOfTasks)
    val rr = new RoundRobinIterator(numOfTasks)//round robin access to seeds array
    val updatedRDD = rdd.map(d => DistributedDatum(d.algorithm, seeds(rr.next()), d.stoppingCondition))
    updatedRDD
  }
}