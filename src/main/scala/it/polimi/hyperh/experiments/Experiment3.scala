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
class Experiment3(instance: Int) 
extends Experiment() {
  override def run() {
    val runs = 1
    val problem = PfsProblem.fromResources(filename("inst_ta", instance, ".txt"))
    val makeAlgo = () => new IGAlgorithm()
    val numOfAlgorithms = 20
    val totalTime = problem.getExecutionTime()*20
    val numOfIterations = 10
    val iterTimeLimit = totalTime / numOfIterations
    val stopCond = new TimeExpired(iterTimeLimit)
    val strategy = new SameSeeds()
    
    val logStartTime = CurrentTime()
    val logname = logStartTime.toString()
    logger.printInfo("Start time\t\t"+logStartTime+"\n")
    logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"))
    val format = logger.getFormatString()
    logger.printInfo(format)
    val conf = new FrameworkConf()
      .setProblem(problem)
      .setNAlgorithms(makeAlgo, numOfAlgorithms)
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
object Experiment3 {
  def main(args: Array[String]) {
    var instance = 1
    //parse the args
    if(args.length != 1)
      throw new RuntimeException("One argument should be provided -> [instanceNumber: Int]")
    else if(args.length >= 1){
      println("Initializing parameters...")
      instance = args(0).toInt
    }
    else {
      println("Not enough arguments passed. Using default Framework configuration...")
    }
    (new Experiment3(instance)).run()
  }
}