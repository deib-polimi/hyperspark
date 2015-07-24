package it.polimi.hyperh.problem

import Array._
import scala.io.Source
import it.polimi.hyperh._
import it.polimi.hyperh.Types._
import util.ProblemParser
import solution.Solution
import solution.EvaluatedSolution



@SerialVersionUID(100L)
class Problem  (
	n:Int,					//num of machines
	m:Int,					//num of jobs
	d: Array[Array[Int]] 	// delays(i)(j) of j-th job on i-th machine (0<=i<n) (0<=j<m)
) extends Serializable
{
	val numOfMachines = n
	val numOfJobs = m
	val jobTimesMatrix = d
	
  
  //associate sequentially job key to array of times present in matrix
  def associateJobToArray(jobs: Array[Int], jobTimesMatrix: Array[Array[Int]]) = {
    val array = new Array[Int](numOfMachines)
    
    jobs zip jobTimesMatrix
  }
  //returns array of times when provided the job value (key)
  def getTimesArrayByJobKey(job: Int, pairs: Array[(Int, Array[Int])]) = pairs.filter(_._1 == job).map(pair => pair._2).flatten

  //calculate end (completion) times for all jobs on each machine
  def jobsInitialTimes(): Array[Array[Int]] = {
    val resultingMatrix = Array.ofDim[Int](numOfMachines, numOfJobs)
    resultingMatrix(0) = jobTimesMatrix(0)
    for(jInd <- 0 until numOfJobs; mInd <- 1 until numOfMachines) {
      resultingMatrix(mInd)(jInd) = resultingMatrix(mInd-1)(jInd)+jobTimesMatrix(mInd)(jInd)   
    }
    resultingMatrix
  }
  //extracts end times from initEndTimesMatrix, e.g. Array(33, 31, 32, 29, 24)
  def extractEndTimes(matrix: Array[Array[Int]]): Array[Int] = {
    matrix(matrix.size-1)
  }
  //associates job key to value of its end time.e.g. Array((1,33), (2,31), (3,32), (4,29), (5,24))
  def createJobValuePairs(jobs: Array[Int], times: Array[Int]) = {
    jobs zip times
  }
  //sort pairs of (jobkey,endTime) by second parameter, in a decreasing order
  def sortJobsDecreasing(pairs: Array[(Int, Int)]) = {
      pairs.sortBy(_._2).reverse
    }
  //gets first N jobs from a sorted (in decreased order) list of pairs (jobkey,endTime)
  def getWorstNJobs(pairs: Array[(Int, Int)], n: Int) = {
    val sorted = sortJobsDecreasing(pairs)
    pairs.take(n)
  }
  //GLOBAL VARIABLES, //calculated only during initialization of the algorithm
  //global matrix of end times
  val initEndTimesMatrix = jobsInitialTimes()
  val jobs = (1 to numOfJobs) toArray
  //associate job to value of its last init end time
  val pairs = createJobValuePairs(jobs, extractEndTimes(initEndTimesMatrix))
  //associate job to an array of its init end times
  val jobTimesPairs = associateJobToArray(jobs, initEndTimesMatrix)
  
  def evaluatePartialSolution(jobsPermutation: Permutation,jobTimesMatrix:Array[Array[Int]],initEndTimesMatrix:Array[Array[Int]]):EvaluatedSolution = {
     val numOfPartJobs = jobsPermutation.length
     val numOfMachines = jobTimesMatrix.size
      val table = Array.ofDim[Int](numOfMachines, jobsPermutation.length)
      //first job is the same
      for(mInd <- 0 until numOfMachines)
        table(mInd)(0) = initEndTimesMatrix(mInd)(jobsPermutation(0)-1)
      for(jInd <- 1 until numOfPartJobs; mInd <- 0 until numOfMachines) {
        if(mInd>0)
          table(mInd)(jInd) = Math.max(table(mInd-1)(jInd),table(mInd)(jInd-1))+jobTimesMatrix(mInd)(jobsPermutation(jInd)-1)
         else
          table(mInd)(jInd) = table(mInd)(jInd-1)+jobTimesMatrix(mInd)(jobsPermutation(jInd)-1)
      }
      def encapsulate(value:Value,permutation: Array[Int]) = new EvaluatedSolution(value,permutation)
      encapsulate(table(table.size-1).max, jobsPermutation)
  }    
  //insert the kth job at place,which minimises the partial makespan among the k possible ones
   
}

//Problem Factory
object Problem{
	/**
	 * @arg path - path to a file 
	 */	
	def apply(path:String):Option[Problem] = ProblemParser(Source.fromFile(path).getLines().mkString(" x ") + " x ")
  def evaluate(p: Problem, solution: Solution):EvaluatedSolution = {
    val jobsArray = solution.permutation
    val initEndTimesMatrix = p.jobsInitialTimes()
    val evaluatedSolution = p.evaluatePartialSolution(jobsArray,p.jobTimesMatrix,initEndTimesMatrix)
    evaluatedSolution
  }
	
}