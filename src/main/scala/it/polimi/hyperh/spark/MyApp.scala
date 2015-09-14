package it.polimi.hyperh.spark

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.types.Types._
import util.Timeout
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.algorithms.NEHAlgorithm

/**
 * @author Nemanja
 */
object MyApp {
  
  def main(args : Array[String]) {
    
    val conf = new SparkConf().setAppName("HyperH").setMaster("local[4]")
    val sc = new SparkContext(conf)
    //to set positions for insertions using range
    val m = if (args.length > 0) args(0).toInt else 3
    //read the problem
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val p = Problem(path + "inst_ta002").getOrElse(throw new RuntimeException("ParserError"))
    //in how many partitions
    val N = 100
    //val movesRdd = sc.parallelize(NeighbourhoodSearch.generateAllNeighbourhoodMoves(p.numOfJobs)).cache
    val movesRdd = sc.parallelize(NeighbourhoodSearch.generateNRandomNeighbourhoodMoves(p.numOfJobs, N),N).cache
    
    val timeLimit = p.numOfMachines*(p.numOfJobs/2.0)*60//termination is n*(m/2)*60 milliseconds
    val expireTimeMillis = Timeout.setTimeout(timeLimit)
    
    val solution = loop(p, movesRdd, expireTimeMillis)
    println(solution)  
  }
  
  def hyperMap(problem: Problem, solution: List[Int], move: (Int,Int), iter: Int): EvaluatedSolution = {
    problem.evaluatePartialSolution(NeighbourhoodSearch.INSdefineMove(solution, move._1, move._2))
  }
  
  def hyperReduce(sol1: EvaluatedSolution, sol2: EvaluatedSolution): EvaluatedSolution = {
    if(sol1.value < sol2.value)
      new EvaluatedSolution(sol1.value, sol1.solution)
    else
      new EvaluatedSolution(sol2.value, sol2.solution)
  }
  
  def loop(problem: Problem, rddMoves: RDD[(Int,Int)], expireTimeMillis: Double):Solution = {
    def initializeSolution(problem: Problem): EvaluatedSolution = {
      val nehAlgorithm = new NEHAlgorithm()
      nehAlgorithm.evaluate(problem)
    }
    def applyIteration(problem: Problem, solution: List[Int], rddMoves: RDD[(Int,Int)],iter:Int):EvaluatedSolution = {
      rddMoves.map(move=>hyperMap(problem, solution, move, iter)).reduce(hyperReduce(_,_))
    }
    
    def iterloop(rddMoves: RDD[(Int,Int)], iter:Integer, bestSolution: EvaluatedSolution):EvaluatedSolution = 
      if(Timeout.notTimeout(expireTimeMillis)) {
        if(iter == 1) {
          iterloop(rddMoves, iter+1, initializeSolution(problem))
        }
        val newIter = iter+1
        val bestSolutionList = bestSolution.solution.toList
        val neighbourSolution = applyIteration(problem, bestSolutionList , rddMoves , newIter)
        if(neighbourSolution.value < bestSolution.value)
          iterloop(rddMoves, newIter, neighbourSolution)
        else
          iterloop(rddMoves, newIter, bestSolution)
      }
      else {
        bestSolution
      }
    iterloop(rddMoves, 1, new EvaluatedSolution(999999999, problem.jobs))
  }
}