package pfsp.parallel

import scala.reflect.{ClassTag, classTag}
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.spark.StoppingCondition
import it.polimi.hyperh.spark.TimeExpired
import pfsp.problem.PfsProblem
import pfsp.algorithms.HGAAlgorithm
import pfsp.solution.PfsSolution
import pfsp.solution.PfsEvaluatedSolution
import util.Performance
import util.CustomLogger
import util.CurrentTime
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD

/**
 * @author Nemanja
 */
class PHGA(
    p: PfsProblem, 
    popSize: Int,
    val numOfSplits: Int, 
    prob: Double, 
    coolingRate: Double,
    seedOption: Option[PfsSolution]
    ) extends HGAAlgorithm(p,popSize,prob,coolingRate,seedOption) {
  val subPopSize = popSize / numOfSplits
  /**
   * A secondary constructor.
   */
  def this(p: PfsProblem, seedOption: Option[PfsSolution]) {
    this(p, 40, 4, 0.1, 0.95, seedOption)
  }
  def this(p: PfsProblem) {
    //popSize: 40, prob: 0.1, coolingRate: 0.95
    this(p, 40, 4, 0.1, 0.95, None)
  }
  temperatureUB = 2000.0 //dummy initialization
  seed = seedOption
  
  override def evaluate(problem:Problem, stopCond: StoppingCondition): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    //INITIALIZE POPULATION
    val individuals = initSeedPlusRandom(p, popSize)
    //assign enumeration id to each of individuals
    val keyValuesArr = for(id<-0 until individuals.size) yield (id, individuals(id).asInstanceOf[PfsEvaluatedSolution])
    val rdd = PHGA.createRDD(keyValuesArr)
    //map each id into operation number, e.g. 0,1,2 or 3rd operation as key, value is just copied
    var populationRDD = rdd.map(tupple => (tupple._1 / subPopSize, tupple._2))
    //create ordering for performing min and max on rdd
    implicit val orderingPfsEv = new Ordering[PfsEvaluatedSolution] {
      override def compare(a: PfsEvaluatedSolution, b: PfsEvaluatedSolution) = a.compare(b)
    }
    implicit val orderingTupples = new Ordering[(Int, PfsEvaluatedSolution)] {
      override def compare(a: (Int, PfsEvaluatedSolution), b: (Int, PfsEvaluatedSolution)) = a._2.compare(b._2)
    }
    var bestSolution = populationRDD.min()(orderingTupples)._2
    var worstSolution = populationRDD.max()(orderingTupples)._2
    val delta = worstSolution.value - bestSolution.value
    temperatureUB = -delta / scala.math.log(prob)
    
    def getCrossoverOp(num: Int): (List[Int], List[Int]) => (List[Int], List[Int]) = {
      val i = num % 4
      if(i == 0) crossoverLOX
      else if(i == 1) crossoverPMX
      else if(i == 2) crossoverC1
      else crossoverNABEL
    }
    
    val stop = stopCond.asInstanceOf[TimeExpired].initialiseLimit()
    while (stop.isNotSatisfied()) {
      //DIVIDE POPULATION IN numOfSplits SUBPOPULATIONS
          //we have subPopSize value calculated
      val subpopulations: Array[RDD[(Int, PfsEvaluatedSolution)]] = (for(i<-0 until numOfSplits) yield populationRDD.filter(t => t._1 == i)).toArray
      //CROSSOVER
      val newsubpopulations = (for(i<-0 until numOfSplits) yield 
          crossover(p, subpopulations(i), bestSolution, getCrossoverOp(i), stop)).toArray
      //UPDATE POPULATION
          //make a union of all rdds
      populationRDD = newsubpopulations.reduceLeft(_ union _)
      bestSolution = List(populationRDD.min()(orderingTupples)._2, bestSolution).min(orderingPfsEv)
      //METROPOLIS MUTATION
      populationRDD = metropolis(p, populationRDD, stop)
      bestSolution = List(populationRDD.min()(orderingTupples)._2, bestSolution).min(orderingPfsEv)
      
    } //endwhile
    //RETURN BEST SOLUTION
    bestSolution
  }
  def getElementAt(rdd: RDD[(Int, PfsEvaluatedSolution)], index: Int): (Int, PfsEvaluatedSolution) = {
    //rdd = (a,b,c)
    val withIndex = rdd.zipWithIndex // ((a,0),(b,1),(c,2))
    val indexKey = withIndex.map{case (k,v) => (v,k)}  //((0,a),(1,b),(2,c))
    val el = indexKey.lookup(index)(0) // Array(b)
    el
  }
  def crossover(p: PfsProblem, subpopulation: RDD[(Int, PfsEvaluatedSolution)], bestSolution: PfsEvaluatedSolution,
      operator: (List[Int], List[Int]) => (List[Int], List[Int]), stopCond: StoppingCondition): RDD[(Int,PfsEvaluatedSolution)] = {
    var newPopulation = subpopulation
    val Ps: Int = subpopulation.count().toInt
    //parent1 uses bestSolution
    val parent1 = bestSolution
    //select parent2 using uniform distribution
    val tupple = getElementAt(subpopulation, this.random.nextInt(Ps))
    val parent2 = tupple._2
    //apply crossover operator
    val children = operator(parent1.solution.toList, parent2.solution.toList)
    val child1 = (tupple._1, p.evaluate(PfsSolution(children._1)).asInstanceOf[PfsEvaluatedSolution])
    val child2 = (tupple._1, p.evaluate(PfsSolution(children._2)).asInstanceOf[PfsEvaluatedSolution])
    var newPopulationArr: RDD[(Int,PfsEvaluatedSolution)]= newPopulation ++ PHGA.createRDD(Array(child1,child2))
    val sortedRDD = newPopulationArr.sortBy[Int](_._2.value)(Ordering.Int, ClassTag.Int)
    val indexKeyRDD = sortedRDD.zipWithIndex().map{case (k,v) => (v,k)}
    newPopulationArr = indexKeyRDD.filter(tupple => tupple._1 < Ps).map(t => t._2)
    newPopulationArr
  }
  def metropolis(p: PfsProblem, population: RDD[(Int,PfsEvaluatedSolution)], stopCond: StoppingCondition):RDD[(Int,PfsEvaluatedSolution)] = {
    var evOldPopulation = population
    var temperature = temperatureUB
    //function that repeats metropolis sample n times on one element at current temperature
    def metropolisOneElement(el: (Int, PfsEvaluatedSolution),locBest: (Int, PfsEvaluatedSolution), runs: Int): (Int, PfsEvaluatedSolution) = {
      if((runs < p.numOfJobs) && stopCond.isNotSatisfied()) {
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
          val evNewSolution = p.evaluate(PfsSolution(newSolution)).asInstanceOf[PfsEvaluatedSolution]
            
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
    while (iter < p.numOfJobs && stopCond.isNotSatisfied()) {
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
    val problem = PfsProblem.fromResources("inst_ta"+instance+".txt")
    val n = problem.numOfJobs
    val m = problem.numOfMachines
    val totalTime = problem.getExecutionTime()
    val stopCond = new TimeExpired(totalTime)
    val bestSolution = PfsEvaluatedSolution.fromResources("sol_ta"+instance+".txt")
    val algorithm = new PHGA(problem)
    val solution = algorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val rpd = Performance.RPD(solution.value, bestSolution.value)
    val logger = CustomLogger() 
    val logname = CurrentTime()
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