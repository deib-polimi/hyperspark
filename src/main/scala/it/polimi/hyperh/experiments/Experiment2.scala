package it.polimi.hyperh.experiments

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.spark.TimeExpired
import it.polimi.hyperh.spark.SameSeeds
import pfsp.problem.PfsProblem
import pfsp.algorithms.HGAAlgorithm
import pfsp.util.PermutationUtility
import util.Performance
import util.CustomLogger
import util.CurrentTime
import pfsp.algorithms.IGAlgorithm
import pfsp.spark.SeedPlusSlidingWindow
import pfsp.spark.SeedPlusFixedWindow

/**
 * @author Nemanja
 */
class Experiment2(instance: Int, parallelism: Int, algName: String, strat: String) 
extends Experiment(instance, parallelism) {
  override def run() {
    val runs = 1
    val problem = PfsProblem.fromResources(filename("inst_ta", instance, ".txt"))
    val algorithm = algName match {
      case "HGAAlgorithm" => new HGAAlgorithm(problem)
      case "IGAlgorithm" => new IGAlgorithm()
      case _ => throw new RuntimeException("Invalid algorithm name")
    }
    val numOfAlgorithms = parallelism
    val totalTime = problem.getExecutionTime()
    val numOfIterations = 10
    val iterTimeLimit = totalTime / numOfIterations
    val stopCond = new TimeExpired(iterTimeLimit)
    val windowSize: Int = scala.math.sqrt(problem.numOfJobs).toInt
    val strategy = strat match {
      case "SameSeeds" => new SameSeeds()
      case "SeedPlusSlidingWindow" => new SeedPlusSlidingWindow(windowSize)
      case "SeedPlusFixedWindow" => new SeedPlusFixedWindow(windowSize)
      case _ => throw new RuntimeException("Invalid seeding strategy name")
    }
    
    val logStartTime = CurrentTime()
    val logname = logStartTime.toString()
    logger.printInfo("Start time\t\t"+logStartTime+"\n")
    logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"))
    val format = logger.getFormatString()
    logger.printInfo(format)
    val conf = new FrameworkConf()
      .setProblem(problem)
      .setNAlgorithms(algorithm, numOfAlgorithms)
      .setNDefaultInitialSeeds(numOfAlgorithms)
      .setNumberOfIterations(numOfIterations)
      .setStoppingCondition(stopCond)
      .setSeedingStrategy(strategy)
    val resultStr = testInstance(instance, runs, conf, true)
    logger.printInfo(resultStr)
    val logEndTime = CurrentTime()
    val strEnd = "End time\t\t"+logEndTime+"\n"
    logger.printInfo(strEnd)
    val duration = logEndTime.diffInSeconds(logStartTime)
    logger.printInfo("Duration\t\t"+duration+"s\n")

  }
}
object Experiment2 {
  def main(args: Array[String]) {
    var instance = 1
    var parallelism = 1
    var algName= "HGAAlgorithm"
    var strat="SameSeeds"
    //parse the args
    if(args.length != 0 && args.length != 4)
      throw new RuntimeException("Four arguments should be provided -> [instanceNumber: Int], [parallelismLevel: Int], [algName:String] and [strategy:String].")
    else if(args.length >= 4){
      println("Initializing parameters...")
      instance = args(0).toInt
      parallelism = args(1).toInt
      algName = args(2)
      strat=args(3)
    }
    else {
      println("Not enough arguments passed. Using default Framework configuration...")
    }
    (new Experiment2(instance, parallelism, algName,strat)).run()
  }
}