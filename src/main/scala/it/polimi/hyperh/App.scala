package it.polimi.hyperh

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

/**
 * @author ${user.name}
 */
object App {
  
  type Solution = List[Int]
  
  def hyperMap(value: Int, position: Int, problem: Problem): List[Int] = 1 to problem.n toList
  
  def hyperReduce(sol1: Solution, sol2: Solution,  problem: Problem): Solution = (sol1, sol2).zipped.map(Math.max(_,_))
  
  def main(args : Array[String]) {
    val conf = new SparkConf().setAppName("HyperH").setMaster("local[4]")
    val sc = new SparkContext(conf)
    
    val n = if (args.length > 0) args(0).toInt else 2
    val initial = sc.parallelize(1 until n,n).cache
    
    val problem = new Problem(n,x=>0)
    val solution = loop(initial,problem)
    
    println(solution)
    
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
