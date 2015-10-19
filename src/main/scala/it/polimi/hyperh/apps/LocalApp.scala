package it.polimi.hyperh.apps

import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.algorithms.PACOAlgorithm
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.spark.Framework


/**
 * @author Nemanja
 */
class LocalApp {
  def run() {
    val problem = Problem.fromResources("inst_ta002.txt")
    val algorithm = new PACOAlgorithm(problem)
    val numOfAlgorithms = 4
    val conf = new FrameworkConf()
    .setDeploymentLocalNumExecutors(numOfAlgorithms)
    .setProblem(problem)
    .setNAlgorithms(algorithm, numOfAlgorithms)
    .setNDefaultInitialSeeds(numOfAlgorithms)
    .setDefaultExecutionTimeLimit()
    
    val solution = Framework.run(conf)
    println(solution)
    //Performance.RPD(solution, optVal)
  }
}
object LocalApp {
  def main(args: Array[String]) {
    new LocalApp().run()
  }
}