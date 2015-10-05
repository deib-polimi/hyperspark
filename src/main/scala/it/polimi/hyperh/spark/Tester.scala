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
import it.polimi.hyperh.algorithms.IGAlgorithm
import util.Performance
import util.FileManager
/**
 * @author Nemanja
 */
object Tester {
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
  def main(args: Array[String]) {
    val runs = 10
    val algorithm = new IGAlgorithm()
    val numOfAlgorithms = 4
    val format = "instance\tn\tm\talgorithmName\t\tparallelism\ttotalTime(s)\tmakespan\tbest\trpd\t\tmode\n"
    print(format)
    FileManager.write("./resources/a-results.txt", format)
    var results: Array[String] = Array(format)
    for (i <- 1 to 120) {
      val problem = Problem("./resources/" + filename("inst_ta", i, ".txt"))
      val conf = new FrameworkConf()
        .setDeploymentLocalNumExecutors(numOfAlgorithms)
        .setProblem(problem)
        .setNAlgorithms(algorithm, numOfAlgorithms)
        .setNDefaultInitialSeeds(numOfAlgorithms)
        .setDefaultExecutionTimeLimit()
      val resultStr = testInstance(i, runs, conf, true)
      results = results ++ Array(resultStr)
      FileManager.append("./resources/a-results.txt", resultStr)
    }
    FileManager.write("./resources/a-results.txt", results.mkString)

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
      bestSolution = EvaluatedSolution("./resources/" + filename("sol_ta", i, ".txt"))
    } else {
      bestSolution = solutions.min
    }
    for (j <- 0 until solutions.size) {
      val rpd = Performance.RPD(solutions(j), bestSolution)
      val newString: String = 
        filename("inst_ta", i, "") + "\t" +
        n.toString + "\t" +
        m.toString + "\t" +
        algName + "\t\t\t" +
        parallelism + "\t\t\t" +
        totalTime / 1000.0 + "\t\t\t\t" +
        solutions(j).value + "\t\t" +
        bestSolution.value + "\t" +
        formatNum(rpd) + "\t" +
        mode + "\n"
      print(newString)
      resString = resString + newString
      //rpds :+= rpd
    }
    //val arpd = Performance.ARPD(rpds)
    resString
  }

}