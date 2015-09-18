package it.polimi.hyperh

import it.polimi.hyperh.spark.FrameworkConf
import it.polimi.hyperh.algorithms.PACOAlgorithm
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.spark.Framework


/**
 * @author Nemanja
 */
object App {
  def main(args : Array[String]) {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta002").getOrElse(throw new RuntimeException("ParserError"))
    val algorithm = new PACOAlgorithm(problem)
    val conf = new FrameworkConf()
    .setSparkMaster("local[4]")
    .setProblem(problem)
    .setNumberOfNodes(4)
    .setAllAlgorithms(algorithm)
    .setDefaultSeeds()
    .setDefaultExecutionTimeLimit()
    
    Framework.run(conf)
  }
}
