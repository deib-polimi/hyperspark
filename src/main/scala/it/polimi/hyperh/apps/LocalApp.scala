package it.polimi.hyperh.apps

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.spark.TimeExpired
import pfsp.problem.PfsProblem
import pfsp.algorithms.IGAlgorithm


/**
 * @author Nemanja
 */
object LocalApp {
  def main(args: Array[String]) {
    val problem = PfsProblem.fromResources("inst_ta002.txt")
    val algorithm = new IGAlgorithm()
    val numOfAlgorithms = 4
    val totalTime = problem.getExecutionTime()
    val numOfIterations = 1
    val iterTimeLimit = totalTime / numOfIterations
    val stopCond = new TimeExpired(iterTimeLimit)
    
    val conf = new FrameworkConf()
    .setDeploymentLocalNumExecutors(numOfAlgorithms)
    .setProblem(problem)
    .setNAlgorithms(algorithm, numOfAlgorithms)
    .setNDefaultInitialSeeds(numOfAlgorithms)
    .setNumberOfIterations(numOfIterations)
    .setStoppingCondition(stopCond)
    
    val solution = Framework.run(conf)
    println(solution)
  }
}