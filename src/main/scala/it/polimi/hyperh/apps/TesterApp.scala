package it.polimi.hyperh.apps

import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.spark.TimeExpired
import pfsp.algorithms.IGAlgorithm
import pfsp.problem.PfsProblem
import pfsp.solution.BadPfsEvaluatedSolution
import pfsp.solution.PfsEvaluatedSolution
import util.Performance
import util.FileManager
import util.CustomLogger
import util.CurrentTime
import java.io.File

/**
 * @author Nemanja
 */
class TesterApp {
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
    val logname = CurrentTime()
    
    logger.printInfo("Start time\t\t"+logname+"\n")
    logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"))
    val format = logger.getFormatString()
    logger.printInfo(format)
    FileManager.write("./output/"+logname+".txt", format)
    var results: Array[String] = Array(format)
    for (i <- 1 to 120) {
      val problem = PfsProblem.fromResources(filename("inst_ta", i, ".txt"))
      val totalTime = problem.getExecutionTime()
      val numOfIterations = 1
      val iterTimeLimit = totalTime / numOfIterations
      val stopCond = new TimeExpired(iterTimeLimit)
      
      val conf = new FrameworkConf()
        .setDeploymentYarnCluster()//.setDeploymentLocalNumExecutors(numOfAlgorithms)
        .setProblem(problem)
        .setNAlgorithms(algorithm, numOfAlgorithms)
        .setNDefaultInitialSeeds(numOfAlgorithms)
        .setNumberOfIterations(numOfIterations)
        .setStoppingCondition(stopCond)
      val resultStr = testInstance(i, runs, conf, true)
      results :+= resultStr
      FileManager.append("./output/"+logname+".txt", resultStr)
      logger.printInfo(resultStr)
    }
    //FileManager.write("./output/"+logname+".txt", results.mkString)
    logger.printInfo("End time\t\t"+CurrentTime()+"\n")

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
    val problem = conf.getProblem().asInstanceOf[PfsProblem]
    var bestSolution = BadPfsEvaluatedSolution(problem)
    val n = problem.numOfJobs
    val m = problem.numOfMachines
    val algName = conf.getAlgorithms().apply(0).name //take first alg name
    val parallelism = conf.getAlgorithms().size
    val iterTimeLimit = conf.getStoppingCondition().asInstanceOf[TimeExpired].getLimit()
    val totalTime = iterTimeLimit * conf.getNumberOfIterations()
    //var rpds: List[Double] = List()
    val solutions = Framework.multipleRuns(conf, runs)
    if (solutionPresent) {
      bestSolution = PfsEvaluatedSolution.fromResources(filename("sol_ta", i, ".txt"))
    } else {
      bestSolution = solutions.min.asInstanceOf[PfsEvaluatedSolution]
    }
    for (j <- 0 until solutions.size) {
      val rpd = Performance.RPD(solutions(j).asInstanceOf[PfsEvaluatedSolution], bestSolution)
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
      //rpds :+= rpd
    }
    //val arpd = Performance.ARPD(rpds)
    resString
  }

}
object TesterApp {
  def main(args: Array[String]) {
     new TesterApp().run()
  }
}