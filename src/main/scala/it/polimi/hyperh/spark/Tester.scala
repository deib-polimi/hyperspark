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
import util.RNG
import util.Performance
import util.FileManager
/**
 * @author Nemanja
 */
object Tester {
  def filename(prefix: String, i: Int, sufix: String) = {
      val str = i.toString
      str.size match {
        case 1 => prefix+"00"+str+sufix
        case 2 => prefix+"0"+str+sufix
        case _ => prefix+str+sufix
      }
    }
  def main(args : Array[String]) {
    val runs = 10
    val algorithm = new IGAlgorithm()
    val numOfAlgorithms = 4
    //var results: Array[String] = Array()
    for(i <- 1 to 120) {
      val problem = Problem("./resources/"+filename("inst_ta",i,".txt"))
      val conf = new FrameworkConf()
      .setDeploymentLocalNumExecutors(numOfAlgorithms)
      .setProblem(problem)
      .setNAlgorithms(algorithm, numOfAlgorithms)
      .setNDefaultSeeds(numOfAlgorithms)
      .setDefaultExecutionTimeLimit()
      //results = results ++ Array(testInstance(i, runs, conf))
      FileManager.write("./resources/a_result_"+i+".txt", testInstance(i, runs, conf))
    }
    //FileManager.write("./resources/a-results.txt", results.mkString)
    
  }
  def testInstance(i: Int, runs: Int, conf: FrameworkConf) = {
    val bestSolution = EvaluatedSolution("./resources/"+filename("sol_ta",i,".txt"))
    var rpds: List[Double] = List()
    val solutions = Framework.multipleRuns(conf, runs)
    for(i <- 0 until solutions.size){
      val rpd = Performance.RPD(solutions(i), bestSolution)
      rpds = rpds ::: List(rpd)
    }
    val arpd = Performance.ARPD(rpds)
    val algorithmName = conf.getAlgorithms().apply(0).name//take first alg name
    val numOfExecutors = conf.getAlgorithms().size
    val resString = filename("inst_ta",i,"")+" "+numOfExecutors+"x "+algorithmName+" ARPD: "+arpd+"\n"
    resString
  }
  

  
}