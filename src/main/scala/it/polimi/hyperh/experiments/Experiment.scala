package it.polimi.hyperh.experiments

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Timeout
import it.polimi.hyperh.solution.DummyEvaluatedSolution
import util.Performance
import util.Timeout
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
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
abstract class Experiment(instance: Int, parallelism: Int) {
  /**
   * Default constructor
   */
  def this() {
    this(1, 1)
  }
  protected val logger = CustomLogger()

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
        mode))
      resString = resString + newString
    }
    resString
  }
  def run(): Unit

}