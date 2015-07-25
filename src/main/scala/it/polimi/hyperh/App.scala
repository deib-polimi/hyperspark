package it.polimi.hyperh

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.types.Types._

/**
 * @author ${user.name}
 */
object App {
  
  
  def hyperMap(value: Int, position: Int, problem: Problem): Solution = new Solution(1 to problem.numOfJobs toArray)
  
  def hyperReduce(sol1: Solution, sol2: Solution,  problem: Problem): Solution = new Solution ((sol1.permutation, sol2.permutation).zipped.map(Math.max(_,_)))
  
  def main(args : Array[String]) {
    //val conf = new SparkConf().setAppName("HyperH").setMaster("local[4]")
    //val sc = new SparkContext(conf)
    
   // val m = if (args.length > 0) args(0).toInt else 3
   // val initial = sc.parallelize(1 until m,m).cache
    
//    val problem = new Problem(3,4)
//    
//    val sol = new Solution(List(2,3,1).toArray)
//    
    //val solution = loop(initial,problem)
    //println(solution)
    
  }
  
  def loop(rdd: RDD[Int], problem: Problem):Solution = {
    def applyIteration(rdd: RDD[Int],iter:Int):Solution = rdd.map(x=>hyperMap(x, iter, problem)).reduce(hyperReduce(_,_,problem))
    def goodEnough(solution:Solution):Boolean = true
    def iterloop(rdd: RDD[Int], iter:Integer, solution: Solution):Solution = 
      if(goodEnough(solution)) solution
      else {
        val newIter = iter+1
        iterloop(rdd,newIter,applyIteration(rdd,newIter))
      }
    iterloop(rdd, 1, applyIteration(rdd,1))
  }
  
  
 
  

}
