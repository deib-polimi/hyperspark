package it.polimi.hyperh.experiments

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.solution.DummyEvaluatedSolution
import it.polimi.hyperh.algorithms.HGAAlgorithm
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import util.Performance
import util.Timeout
import util.CustomLogger

/**
 * @author Nemanja
 */
class Experiment1(instance: Int, parallelism: Int) extends Experiment(instance, parallelism) {
  
   override def run() {
    val runs = 1
    val problem = Problem.fromResources(filename("inst_ta", instance, ".txt"))
    val algorithm = new HGAAlgorithm(problem)
    val numOfAlgorithms = parallelism
    val startTime = Timeout.getCurrentTime()
    val logname = startTime.toString()
    logger.printInfo("Start time\t\t"+startTime+"\n")
    logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"))
    val format = logger.getFormatString()
    logger.printInfo(format)
    val conf = new FrameworkConf()
      .setDeploymentYarnCluster()
      .setProblem(problem)
      .setNAlgorithms(algorithm, numOfAlgorithms)
      .setNDefaultInitialSeeds(numOfAlgorithms)
      .setDefaultExecutionTimeLimit()
      val resultStr = testInstance(instance, runs, conf, true)
    logger.printInfo(resultStr)
    val endTime = Timeout.getCurrentTime()
    val strEnd = "End time\t\t"+endTime+"\n"
    logger.printInfo(strEnd)
    val duration = endTime.diffInSeconds(startTime)
    logger.printInfo("Duration\t\t"+duration+"s\n")

  }
}
object Experiment1 {
  def main(args: Array[String]) {
    var instance = 1
    var parallelism = 1
    //parse the args
    if(args.length != 0 && args.length != 2)
      throw new RuntimeException("Two arguments should be provided -> [instanceNumber: Int] and [parallelismLevel: Int].")
    else if(args.length >= 2){
      println("Initializing parameters...")
      instance = args(0).toInt
      parallelism = args(1).toInt
      
    }
    else {
      println("Not enough arguments passed. Using default Framework configuration...")
    }
    new Experiment1(instance, parallelism).run()
  }
}