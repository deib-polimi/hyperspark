package it.polimi.hyperh.spark
import it.polimi.hyperh.algorithms.Algorithm
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.problem.Problem
/**
 * @author Nemanja
 */
class FrameworkConf() {
  private var numOfNodes = 1
  private var algs: Array[Algorithm] = Array()
  private var sds: Array[Option[Solution]] = Array()
  private var mUrl: String = "local"  //1 core
  private var problem: Problem = new Problem(3,3,Array(Array(1,2,1), Array(1,1,2),Array(2,2,1)))//dummy
  private var tLimit: Double = 3000
  private var iter: Int = 1
  
  def setNumberOfNodes(n: Int) = {
    numOfNodes = n
    this
  }
  def getNumberOfNodes() = {numOfNodes }
  
  def setAlgorithms(algorithms: Array[Algorithm]) = { 
    algs = algorithms
    this
  }
  def getAlgorithms() = algs.clone()
  
  def setSeeds(seeds: Array[Option[Solution]]) = { 
    sds = seeds
    this
  }
  def getSeeds() = sds.clone()
  
  def appendAlgorithm(algorithm: Algorithm) = {
    if(algs.size < numOfNodes)
      algs :+= algorithm
    else
      println("WARNING: Cannot append more algorithms. The limit is the number of phisical nodes.")
    this
  }
  def setAllAlgorithms(algorithm: Algorithm) = {
    algs = Array.fill(numOfNodes)(algorithm)
    this
  }
  def clearAlgorithms() = { 
    algs = Array()
    this
  }
  
  def appendSeed(seed: Option[EvaluatedSolution]) = {
    if(sds.size < numOfNodes)
      sds :+= seed
    else
      println("WARNING: Cannot append more seeds. The limit is the number of phisical nodes.")
    this
  }
  def clearSeeds() = { 
    sds = Array()
    this
  }
  def setDefaultSeeds() = {
    sds = Array.fill(numOfNodes)(None)
    this
  }
  
  def setSparkMaster(url: String) = { 
    mUrl = url
    this
  }
  def getSparkMaster(): String = { mUrl }
  def getNumOfNodes(): Int = { numOfNodes }
  
  def setDefaultExecutionTimeLimit() = {
    tLimit = problem.numOfMachines*(problem.numOfJobs/2.0)*60//termination is n*(m/2)*60 milliseconds
    this
  }
  def setIterationTimeLimit(millis: Double) = {
    tLimit = millis
    this
  }
  def getIterationTimeLimit() = {
    tLimit
  }
  def setProblem(p: Problem) = {
    problem = p
    this
  }
  def getProblem() = { problem }
  
  def setNumberOfIterations(n: Int) = {
    iter = n
    this
  }
  def getNumberOfIterations() = { iter }
}