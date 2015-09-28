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
/**
 * @author Nemanja
 */
object Tester {
  def filename(prefix: String, i: Int) = {
      val str = i.toString
      str.size match {
        case 1 => prefix+"00"+str+".txt"
        case 2 => prefix+"0"+str+".txt"
        case _ => prefix+str+".txt"
      }
    }
  def main(args : Array[String]) {
    val pInd = 1
    val runs = 10
    val problem = Problem("./resources/"+filename("inst_ta",pInd))
    val bestSolution = EvaluatedSolution("./resources/"+filename("sol_ta",pInd))
    val rng = RNG()
    val algorithm = new IGAlgorithm(rng)
    val numOfAlgorithms = 4
    val conf = new FrameworkConf()
    .setDeploymentLocalNumExecutors(numOfAlgorithms)
    .setProblem(problem)
    .setNAlgorithms(algorithm, numOfAlgorithms)
    .setNDefaultSeeds(numOfAlgorithms)
    .setDefaultExecutionTimeLimit()
    
    testInstance(pInd, runs, conf)
  }
  def testInstance(i: Int, runs: Int, conf: FrameworkConf) = {
    val bestSolution = EvaluatedSolution("./resources/"+filename("sol_ta",i))
    var rpds: List[Double] = List()
    val solutions = Framework.multipleRuns(conf, runs)
    for(i <- 0 until solutions.size){
      val rpd = Performance.RPD(solutions(i), bestSolution)
      rpds = rpds ::: List(rpd)
    }
    val arpd = Performance.ARPD(rpds)
    val algorithmName = conf.getAlgorithms().apply(0).name//take first alg name
    val numOfExecutors = conf.getAlgorithms().size
    println(filename("inst_ta",i)+" "+numOfExecutors+"x "+algorithmName+" ARPD: "+arpd)
  }
  

  
}