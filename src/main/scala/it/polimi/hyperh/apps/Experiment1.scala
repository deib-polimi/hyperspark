package it.polimi.hyperh.apps

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Timeout
import it.polimi.hyperh.solution.DummyEvaluatedSolution
import it.polimi.hyperh.algorithms.IGAlgorithm
import util.Performance
import util.FileManager
import util.Timeout
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import java.io.File
import util.CustomLogger
import util.CustomLogger
import it.polimi.hyperh.algorithms.HGAAlgorithm
import it.polimi.hyperh.problem.Problem
import util.Timeout
import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.solution.DummyEvaluatedSolution
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Performance

/**
 * @author Nemanja
 */
object Experiment1 {
  private var instance: Int = 1
  private var parallelism: Int = 1
  private val logger = CustomLogger() 
  
  def main(args: Array[String]) {
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
    this.run()
  }
  
   def run() {
    val runs = 1
    val problem = Problem.fromResources("inst_ta" + instance.toString + ".txt")
    val algorithm = new HGAAlgorithm(problem)
    val numOfAlgorithms = parallelism
    val logname = Timeout.getCurrentTime()
    logger.printInfo("Start time\t\t"+logname+"\n")
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
    val strEnd = "End time\t\t"+Timeout.getCurrentTime()+"\n"
    logger.printInfo(strEnd, true)

  }
  def testInstance(i: Int, runs: Int, conf: FrameworkConf, solutionPresent: Boolean = false) = {
    def getMode() = {
      val usesTheSeed: Boolean = conf.getSeedingStrategy().usesTheSeed()
      val numOfIterations: Int = conf.getNumberOfIterations()
      if (usesTheSeed && numOfIterations > 1)
        "cooperative"
      else "parallel"
    }
    val mode = getMode()
    var resString = ""
    val problem = conf.getProblem()
    var bestSolution = DummyEvaluatedSolution(problem)
    val n = problem.numOfJobs
    val m = problem.numOfMachines
    val algName = conf.getAlgorithms().apply(0).name //take first alg name
    val parallelism = conf.getAlgorithms().size
    val totalTime = conf.getIterationTimeLimit() * conf.getNumberOfIterations()
    val solutions = Framework.multipleRuns(conf, runs)
    if (solutionPresent) {
      bestSolution = EvaluatedSolution.fromResources(filename("sol_ta", i, ".txt"))
    } else {
      bestSolution = solutions.min
    }
    for (j <- 0 until solutions.size) {
      val rpd = Performance.RPD(solutions(j), bestSolution)
      val newString = logger.getValuesString(List(
        filename("inst_ta", i, ""),
        n, 
        m,
        algName,
        parallelism,
        totalTime / 1000.0,
        solutions(j).value,
        bestSolution.value,
        formatNum(rpd),
        mode
      ))
      resString = resString + newString
    }
    resString
  }
  def filename(prefix: String, i: Int, sufix: String) = {
    val str = i.toString
    str.size match {
      case 1 => prefix + "00" + str + sufix
      case 2 => prefix + "0" + str + sufix
      case _ => prefix + str + sufix
    }
  }
  def formatNum(num: Double): String = {
    val str = num.toString()
    str.size match {
      case 3 => str + "0"
      case _ => str
    }
  }
}