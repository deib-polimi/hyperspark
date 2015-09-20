package it.polimi.hyperh

import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.algorithms.PACOAlgorithm
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.spark.Framework
import util.Performance


/**
 * @author Nemanja
 */
object App {
  def main(args : Array[String]) {
    val problem = Problem("./resources/inst_ta002")
    val algorithm = new PACOAlgorithm(problem)
    val conf = new FrameworkConf()
    .setSparkMaster("local[4]")
    .setProblem(problem)
    .setNumberOfNodes(4)
    .setAllAlgorithms(algorithm)
    .setDefaultSeeds()
    .setDefaultExecutionTimeLimit()
    
    val solution = Framework.run(conf)
    println(solution)
    //Performance.RPD(solution, optVal)
  }
}
