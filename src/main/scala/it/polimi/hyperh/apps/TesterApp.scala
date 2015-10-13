package it.polimi.hyperh.apps

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Timeout
import it.polimi.hyperh.solution.DummyEvaluatedSolution
import it.polimi.hyperh.algorithms.IGAlgorithm
import util.Performance
import util.FileManager
import util.CustomLogger
import util.Timeout
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import java.io.File

/**
 * @author Nemanja
 */
object TesterApp {
  val logger = CustomLogger() 
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
  def run() {
    val runs = 10
    val algorithm = new IGAlgorithm()
    val numOfAlgorithms = 4
    val logname = Timeout.getCurrentTime()
    logger.printInfo("Start time\t\t"+logname+"\n")
    logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"))
    val format = logger.getFormatString()
    logger.printInfo(format)
    FileManager.write("./output/"+logname+".txt", format)
    var results: Array[String] = Array(format)
    for (i <- 1 to 120) {
      val problem = Problem.fromResources(filename("inst_ta", i, ".txt"))
      val conf = new FrameworkConf()
        .setDeploymentYarnCluster()//.setDeploymentLocalNumExecutors(numOfAlgorithms)
        .setProblem(problem)
        .setNAlgorithms(algorithm, numOfAlgorithms)
        .setNDefaultInitialSeeds(numOfAlgorithms)
        .setDefaultExecutionTimeLimit()
      val resultStr = testInstance(i, runs, conf, true)
      results :+= resultStr
      FileManager.append("./output/"+logname+".txt", resultStr)
      logger.printInfo(resultStr)
    }
    //FileManager.write("./output/"+logname+".txt", results.mkString)

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
    //var rpds: List[Double] = List()
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
      logger.printInfo(newString)
      resString = resString + newString
      //rpds :+= rpd
    }
    //val arpd = Performance.ARPD(rpds)
    resString
  }

}