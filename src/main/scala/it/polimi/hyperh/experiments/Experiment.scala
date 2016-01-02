package it.polimi.hyperh.experiments


import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import pfsp.problem.PfsProblem
import pfsp.solution.PfsEvaluatedSolution
import pfsp.solution.BadPfsEvaluatedSolution
import pfsp.algorithms.HGAAlgorithm
import util.Performance
import util.CustomLogger
import it.polimi.hyperh.spark.TimeExpired

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
    val problem = conf.getProblem().asInstanceOf[PfsProblem]
    var bestSolution = BadPfsEvaluatedSolution(problem)
    val n = problem.numOfJobs
    val m = problem.numOfMachines
    val algName = conf.getAlgorithms().apply(0).name //take first alg name
    val parallelism = conf.getAlgorithms().size
    val iterTimeLimit = conf.getStoppingCondition().asInstanceOf[TimeExpired].getLimit()
    val totalTime = iterTimeLimit * conf.getNumberOfIterations()
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
        mode))
      resString = resString + newString
    }
    resString
  }
  def run(): Unit

}