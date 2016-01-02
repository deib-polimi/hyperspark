package it.polimi.hyperh.experiments

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import pfsp.problem.PfsProblem
import pfsp.algorithms.HGAAlgorithm
import util.Performance
import util.CustomLogger
import pfsp.util.PermutationUtility
import util.CurrentTime
import it.polimi.hyperh.spark.TimeExpired

/**
 * @author Nemanja
 */
class Experiment1(instance: Int, parallelism: Int) extends Experiment(instance, parallelism) {
  
   override def run() {
    val runs = 1
    val problem = PfsProblem.fromResources(filename("inst_ta", instance, ".txt"))
    val algorithm = new HGAAlgorithm(problem)
    val numOfAlgorithms = parallelism
    val totalTime = problem.getExecutionTime()
    val numOfIterations = 1
    val iterTimeLimit = totalTime / numOfIterations
    val stopCond = new TimeExpired(iterTimeLimit)
    
    val logStartTime = CurrentTime()
    val logname = logStartTime.toString()
    logger.printInfo("Start time\t\t"+logStartTime+"\n")
    logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"))
    val format = logger.getFormatString()
    logger.printInfo(format)
    val conf = new FrameworkConf()
      .setDeploymentYarnCluster()
      .setProblem(problem)
      .setNAlgorithms(algorithm, numOfAlgorithms)
      .setNDefaultInitialSeeds(numOfAlgorithms)
      .setNumberOfIterations(1)
      .setStoppingCondition(stopCond)
      val resultStr = testInstance(instance, runs, conf, true)
    logger.printInfo(resultStr)
    val logEndTime = CurrentTime()
    val strEnd = "End time\t\t"+logEndTime+"\n"
    logger.printInfo(strEnd)
    val duration = logEndTime.diffInSeconds(logStartTime)
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