package it.polimi.hyperh.apps

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.spark.Framework
import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.spark.TimeExpired
import pfsp.problem.PfsProblem
import pfsp.algorithms.GAAlgorithm


/**
 * @author Nemanja
 */
object LocalApp {
  def main(args: Array[String]) {
    val problem = PfsProblem.fromResources("inst_ta054.txt")
    val makeAlgo = () => new GAAlgorithm()
    val numOfAlgorithms = 4
    val totalTime = problem.getExecutionTime()
    val numOfIterations = 1
    val iterTimeLimit = totalTime / numOfIterations
    val stopCond = new TimeExpired(iterTimeLimit)
    val randomSeed = 118337975

    val conf = new FrameworkConf()
    .setRandomSeed(randomSeed)
    .setDeploymentLocalNumExecutors(numOfAlgorithms)
    .setProblem(problem)
    .setNAlgorithms(makeAlgo, numOfAlgorithms)
    .setNDefaultInitialSeeds(numOfAlgorithms)
    .setNumberOfIterations(numOfIterations)
    .setStoppingCondition(stopCond)
    
    val solution = Framework.run(conf)
    println(solution)
  }
}