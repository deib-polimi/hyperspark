package it.polimi.hyperh.spark
import it.polimi.hyperh.algorithms.Algorithm
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.problem.Problem
/**
 * @author Nemanja
 */
class FrameworkConf() {
  private var algs: Array[Algorithm] = Array()
  private var sds: Array[Option[Solution]] = Array()
  private var problem: Problem = new Problem(3,3,Array(Array(1,2,1), Array(1,1,2),Array(2,2,1)))//dummy
  private var tLimit: Double = 3000
  private var iter: Int = 1
  private var properties: List[(String,String)] = loadDefaults()
  private var handler: MapReduceHandler = new MapReduceHandler()
  
  def setProblem(p: Problem) = {
    problem = p
    this
  }
  def getProblem() = { problem }
  
  def setAlgorithms(algorithms: Array[Algorithm]) = { 
    algs = algorithms
    this
  }
  def getAlgorithms() = algs.clone()
  
  def appendAlgorithm(algorithm: Algorithm) = {
    algs :+= algorithm
    this
  }
  def setNAlgorithms(algorithm: Algorithm, N: Int) = {
    algs = Array.fill(N)(algorithm)
    this
  }
  def clearAlgorithms() = { 
    algs = Array()
    this
  }
  
  def setSeeds(seeds: Array[Option[Solution]]) = { 
    sds = seeds
    this
  }
  def appendSeed(seed: Option[EvaluatedSolution]) = {
    sds :+= seed
    this
  }
  def setNSeeds(seedOption: Option[EvaluatedSolution], N: Int) = {
    sds = Array.fill(N)(seedOption)
    this
  }
  def setNDefaultSeeds(N: Int) = {
    sds = Array.fill(N)(None)
    this
  }
  def clearSeeds() = { 
    sds = Array()
    this
  }
  def getSeeds() = sds.clone()
  
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
  
  def setNumberOfIterations(n: Int) = {
    iter = n
    this
  }
  def getNumberOfIterations() = { iter }
  
  //for properties reference visit
  //http://spark.apache.org/docs/latest/configuration.html#viewing-spark-properties
  
  def setProperty(key: String, value: String) = {
    //remove default entry for provided key
    properties = properties.filterNot{case (k, v) => (k == key)}
    properties = properties :+ (key, value)
    this
  }
  def getProperties() = properties
  
  
  def setSparkMaster(url: String) = {
    setProperty("spark.master", url)
    this
  }
  def getSparkMaster(): String = { 
    val result = properties.filter{case (key, value) => (key == "spark.master")}
    if(result.size != 0)
      result.head._2
    else {
      println("WARN FrameworkConf : Spark master url is not set")
      "(none)"
    }
  }
  def setDeploymentLocalNoParallelism() = { setSparkMaster("local") }
  def setDeploymentLocalMaxCores() = { setSparkMaster("local[*]") }
  def setDeploymentLocalNumExecutors(numExecutors: Int) = { setSparkMaster("local["+numExecutors.toString()+"]") }
  def setDeploymentSpark(host: String, port: Int) = { setSparkMaster("spark://"+host+":"+port.toString()) }
  def setDeploymentSpark(host: String) = { setSparkMaster("spark://"+host+":7077") }
  def setDeploymentMesos(host: String, port: Int) = { setSparkMaster("mesos://"+host+":"+port.toString()) }
  def setDeploymentMesos(host: String) = { setSparkMaster("mesos://"+host+":5050") }
  def setDeploymentYarnClient() = { setSparkMaster("yarn-client") }
  def setDeploymentYarnCluster() = { setSparkMaster("yarn-cluster") }
  
  def setAppName(name: String) = {
    setProperty("spark.app.name", name)
  }
  private def loadDefaults() = {
    List(
        ("spark.master", "local[*]"),
        ("spark.app.name","HyperH")
        )
  }
  def enableDynamicResourceAllocation() = {
    if(getSparkMaster().contains("yarn")) {
      setProperty("spark.shuffle.service.enabled","true")
      setProperty("spark.dynamicAllocation.enabled", "true")
    }
    else {
      println("WARN FrameworkConf : Dynamic Resource Allocation is supported only in Yarn deployment mode.")
      this
    }
  }
  def setMapReduceHandler(h: MapReduceHandler) = { handler = h }
  def getMapReduceHandler(): MapReduceHandler = handler
}
/*object DefaultYarnConf() {
  def apply(algorithms: Array[Algorithm], seeds: Option[Solution], )
}*/