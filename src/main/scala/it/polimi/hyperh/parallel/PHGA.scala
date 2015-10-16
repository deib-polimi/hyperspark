package it.polimi.hyperh.parallel

import scala.reflect.{ClassTag, classTag}
import it.polimi.hyperh.algorithms.HGAAlgorithm
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Timeout
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD
import util.Performance
import util.CustomLogger

/**
 * @author Nemanja
 */
class PHGA(
    p: Problem, 
    popSize: Int,
    val numOfSplits: Int, 
    prob: Double, 
    coolingRate: Double,
    seedOption: Option[Solution]
    ) extends HGAAlgorithm(p,popSize,prob,coolingRate,seedOption) {
  val subPopSize = popSize / numOfSplits
  /**
   * A secondary constructor.
   */
  def this(p: Problem, seedOption: Option[Solution]) {
    this(p, 40, 4, 0.1, 0.95, seedOption)
  }
  def this(p: Problem) {
    //popSize: 40, prob: 0.1, coolingRate: 0.95
    this(p, 40, 4, 0.1, 0.95, None)
  }
  temperatureUB = 2000.0 //dummy initialization
  seed = seedOption
  
  override def evaluate(p:Problem, timeLimit: Double): EvaluatedSolution = {
    //INITIALIZE POPULATION
    val individuals = initSeedPlusRandom(p, popSize)
    //assign enumeration id to each of individuals
    val keyValuesArr = for(id<-0 until individuals.size) yield (id, individuals(id))
    val rdd = PHGA.createRDD(keyValuesArr)
    //map each id into operation number, e.g. 0,1,2 or 3rd operation as key, value is just copied
    var populationRDD = rdd.map(tupple => (tupple._1 / subPopSize, tupple._2))
    //create ordering for performing min and max on rdd
    implicit val orderingTupples = new Ordering[(Int, EvaluatedSolution)] {
      override def compare(a: (Int, EvaluatedSolution), b: (Int, EvaluatedSolution)) = a._2.compare(b._2)
    }
    var bestSolution = populationRDD.min()(orderingTupples)._2
    var worstSolution = populationRDD.max()(orderingTupples)._2
    val delta = worstSolution.value - bestSolution.value
    temperatureUB = -delta / scala.math.log(prob)
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    def getCrossoverOp(num: Int): (List[Int], List[Int]) => (List[Int], List[Int]) = {
      val i = num % 4
      if(i == 0) crossoverLOX
      else if(i == 1) crossoverPMX
      else if(i == 2) crossoverC1
      else crossoverNABEL
    }
    
    while (Timeout.notTimeout(expireTimeMillis)) {
      //DIVIDE POPULATION IN numOfSplits SUBPOPULATIONS
          //we have subPopSize value calculated
      val subpopulations: Array[RDD[(Int, EvaluatedSolution)]] = (for(i<-0 until numOfSplits) yield populationRDD.filter(t => t._1 == i)).toArray
      //CROSSOVER
      val newsubpopulations = (for(i<-0 until numOfSplits) yield 
          crossover(p, subpopulations(i), bestSolution, getCrossoverOp(i), expireTimeMillis)).toArray
      //UPDATE POPULATION
          //make a union of all rdds
      populationRDD = newsubpopulations.reduceLeft(_ union _)
      bestSolution = List(populationRDD.min()(orderingTupples)._2, bestSolution).min
      //METROPOLIS MUTATION
      populationRDD = metropolis(p, populationRDD, expireTimeMillis)
      bestSolution = List(populationRDD.min()(orderingTupples)._2, bestSolution).min
      
    } //endwhile
    //RETURN BEST SOLUTION
    bestSolution
  }
  def getElementAt(rdd: RDD[(Int, EvaluatedSolution)], index: Int): (Int, EvaluatedSolution) = {
    //rdd = (a,b,c)
    val withIndex = rdd.zipWithIndex // ((a,0),(b,1),(c,2))
    val indexKey = withIndex.map{case (k,v) => (v,k)}  //((0,a),(1,b),(2,c))
    val el = indexKey.lookup(index)(0) // Array(b)
    el
  }
  def crossover(p: Problem, subpopulation: RDD[(Int, EvaluatedSolution)], bestSolution: EvaluatedSolution,
      operator: (List[Int], List[Int]) => (List[Int], List[Int]), expireTimeMillis: Double): RDD[(Int,EvaluatedSolution)] = {
    var newPopulation = subpopulation
    val Ps: Int = subpopulation.count().toInt
    //parent1 uses bestSolution
    val parent1 = bestSolution
    //select parent2 using uniform distribution
    val tupple = getElementAt(subpopulation, this.random.nextInt(Ps))
    val parent2 = tupple._2
    //apply crossover operator
    val children = operator(parent1.solution.toList, parent2.solution.toList)
    val child1 = (tupple._1, p.evaluate(Solution(children._1)))
    val child2 = (tupple._1, p.evaluate(Solution(children._2)))
    var newPopulationArr: Array[(Int,EvaluatedSolution)]= newPopulation.collect() ++ Array(child1) ++ Array(child2)
    newPopulationArr = newPopulationArr.sortBy[Int](_._2.value)(Ordering.Int).take(Ps)
    PHGA.createRDD(newPopulationArr)
  }
  def metropolis(p: Problem, population: RDD[(Int,EvaluatedSolution)], expireTimeMillis: Double):RDD[(Int,EvaluatedSolution)] = {
    var evOldPopulation = population
    var temperature = temperatureUB
    //function that repeats metropolis sample n times on one element at current temperature
    def metropolisOneElement(el: (Int, EvaluatedSolution),locBest: (Int, EvaluatedSolution), runs: Int): (Int, EvaluatedSolution) = {
      if((runs < p.numOfJobs) && Timeout.notTimeout(expireTimeMillis)) {
        var newSolution: List[Int] = List()
        var updateEl = el
        var localBest = locBest
        val i = el._1
          //START METROPOLIS SAMPLE ITERATION
          //generate random neighbouring solution
          //i mod 4, 0,1: swap, 2: inv, 3: ins
          val op = i % 4
          if(op == 0 || op == 1)
            newSolution = mutationSWAP(el._2.solution.toList)
          else if(op == 2)
            newSolution = mutationINV(el._2.solution.toList)
          else
            newSolution = mutationINS(el._2.solution.toList)
          //calculate its cost
          val evNewSolution = p.evaluate(Solution(newSolution))
            
          val delta = evNewSolution.value - el._2.value
          //calculate acceptance probability
          val ap = acceptanceProbability(delta, temperature)
          val randomNo = random.nextDouble()
          
          if ((delta <= 0) || (randomNo <= ap)) {
            updateEl = (el._1, evNewSolution)
          }
          //if we found better solution in the neighbourhood, update localBest
          
          localBest = List(updateEl, localBest).minBy(_._2.value)
          //END METROPOLIS SAMPLE ITERATION
          metropolisOneElement(updateEl, localBest, runs+1)
      }
      else locBest
    }
    var iter = 0
    while (iter < p.numOfJobs && Timeout.notTimeout(expireTimeMillis)) {
      evOldPopulation = evOldPopulation.map{ el =>
        val updateEl = metropolisOneElement(el, el, 0)
        temperature = coolingRate*temperature
        updateEl
      }
      iter = iter + 1
    }//end while
    evOldPopulation
  }
}
object PHGA {
  val sc = new SparkContext(new SparkConf().setAppName("PHGA").setMaster("local[*]"))
  def createRDD[T: ClassTag](seq: Seq[T]): RDD[T] = { sc.parallelize(seq) }
  def main(args: Array[String]) {
    val instance = "050"
    val problem = Problem.fromResources("inst_ta"+instance+".txt")
    val n = problem.numOfJobs
    val m = problem.numOfMachines
    val totalTime = problem.numOfMachines * (problem.numOfJobs / 2.0) * 60
    val bestSolution = EvaluatedSolution.fromResources("sol_ta"+instance+".txt")
    val algorithm = new PHGA(problem)
    val solution = algorithm.evaluate(problem, totalTime)
    val rpd = Performance.RPD(solution, bestSolution)
    val logger = CustomLogger() 
    val logname = Timeout.getCurrentTime()
    logger.printInfo("Start time\t\t"+logname+"\n")
    logger.setFormat(List("instance","n","m","algorithmName","totalTime(s)","makespan","best","rpd","mode"))
    val format = logger.getFormatString()
    logger.printInfo(format)
    val resultStr = logger.getValuesString(List(
        "inst_ta"+instance,
        n, 
        m,
        "PHGA",
        totalTime / 1000.0,
        solution.value,
        bestSolution.value,
        rpd,
        "fully parallel"
      ))
      logger.printInfo(resultStr)
  }
}