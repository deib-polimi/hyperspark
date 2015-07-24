package it.polimi.hyperh
import solution._
import it.polimi.hyperh.Types._

object testNEHproblem {
		val numOfJobs = 5                 //> numOfJobs  : Int = 5
	val numOfMachines = 5                     //> numOfMachines  : Int = 5
	
	def jobTimesMatrix = Array(Array(5,9,8,10,1),Array(9,3,10,1,8),Array(9,4,5,8,6),Array(4,8,8,7,2),Array(3,5,6,3,7))
                                                  //> jobTimesMatrix: => Array[Array[Int]]
	
	//associate sequentially job key to array of times present in matrix
  def associateJobToArray(jobs: Array[Int], jobTimesMatrix: Array[Array[Int]]) = jobs zip jobTimesMatrix
                                                  //> associateJobToArray: (jobs: Array[Int], jobTimesMatrix: Array[Array[Int]])Ar
                                                  //| ray[(Int, Array[Int])]
  //returns array of times when provided the job value (key)
  def getTimesArrayByJobKey(job: Int, pairs: Array[(Int, Array[Int])]) = pairs.filter(_._1 == job).map(pair => pair._2).flatten
                                                  //> getTimesArrayByJobKey: (job: Int, pairs: Array[(Int, Array[Int])])Array[Int]
                                                  //| 

  //calculate end (completion) times for all jobs on each machine
  def jobsInitialTimes(matrix: Array[Array[Int]], numOfJobs: Int, numOfMachines: Int): Array[Array[Int]] = {
    //calculates end times of one job on each machine (progressive sum over array).e.g. Array(5, 14, 22, 32, 33)
    def calculateInitEndTimes(array: Array[Int]): Array[Int] = {
      val size = array.length
      var resultingArray = new Array[Int](size)
      var sum = 0
      for (i <- 0 until size) {
        sum += array(i)
        resultingArray(i) = sum
      }
      resultingArray
    }
    val resultingMatrix = Array.ofDim[Int](numOfJobs, numOfMachines)
    for (i <- 0 until matrix.size)
      resultingMatrix(i) = calculateInitEndTimes(matrix(i))
    resultingMatrix
  }                                               //> jobsInitialTimes: (matrix: Array[Array[Int]], numOfJobs: Int, numOfMachines
                                                  //| : Int)Array[Array[Int]]
  //extracts end times from initEndTimesMatrix, e.g. Array(33, 31, 32, 29, 24)
  def extractEndTimes(matrix: Array[Array[Int]]): Array[Int] = {
    matrix map (array => array(array.length - 1))
  }                                               //> extractEndTimes: (matrix: Array[Array[Int]])Array[Int]
  //associates job key to value of its end time.e.g. Array((1,33), (2,31), (3,32), (4,29), (5,24))
  def createJobValuePairs(jobs: Array[Int], times: Array[Int]) = {
    jobs zip times
  }                                               //> createJobValuePairs: (jobs: Array[Int], times: Array[Int])Array[(Int, Int)]
                                                  //| 
  //gets first N jobs from a sorted (in decreased order) list of pairs (jobkey,endTime)
  def getWorstNJobs(pairs: Array[(Int, Int)], n: Int) = {
    //sort pairs of (jobkey,endTime) by second parameter, in a decreasing order
    def sortJobsDecreasing(pairs: Array[(Int, Int)]) = {
      pairs.sortBy(_._2).reverse
    }
    val sorted = sortJobsDecreasing(pairs)
    pairs.take(n)
  }                                               //> getWorstNJobs: (pairs: Array[(Int, Int)], n: Int)Array[(Int, Int)]
  //GLOBAL VARIABLES, //calculated only during initialization of the algorithm
  //global matrix of end times
  val initEndTimesMatrix = jobsInitialTimes(jobTimesMatrix, numOfJobs, numOfMachines)
                                                  //> initEndTimesMatrix  : Array[Array[Int]] = Array(Array(5, 14, 22, 32, 33), A
                                                  //| rray(9, 12, 22, 23, 31), Array(9, 13, 18, 26, 32), Array(4, 12, 20, 27, 29)
                                                  //| , Array(3, 8, 14, 17, 24))
  val jobs = (1 to numOfJobs) toArray             //> jobs  : Array[Int] = Array(1, 2, 3, 4, 5)
  //associate job to value of its last init end time
  val pairs = createJobValuePairs(jobs, extractEndTimes(initEndTimesMatrix))
                                                  //> pairs  : Array[(Int, Int)] = Array((1,33), (2,31), (3,32), (4,29), (5,24))
                                                  //| 
  //associate job to an array of its init end times
  val jobTimesPairs = associateJobToArray(jobs, initEndTimesMatrix)
                                                  //> jobTimesPairs  : Array[(Int, Array[Int])] = Array((1,Array(5, 14, 22, 32, 3
                                                  //| 3)), (2,Array(9, 12, 22, 23, 31)), (3,Array(9, 13, 18, 26, 32)), (4,Array(4
                                                  //| , 12, 20, 27, 29)), (5,Array(3, 8, 14, 17, 24)))
  
  def evaluatePartialSolution(jobsPermutation: Permutation):EvaluatedSolution = {
      //first row is a init time array of job that is first element of permutation
      val firstRow = getTimesArrayByJobKey(jobsPermutation(0), jobTimesPairs)
      //construct table
      var lastCalculatedRow = firstRow
      //table is a matrix in which arrays of end times are stored
      var table = Array.concat(Array(firstRow))
      
      def calculateNextRow(lastRow:Array[Int],initTimes:Array[Int]):Array[Int] = {
        var lastVal = 0
        var newRow = new Array[Int](initTimes.length)
        for(machInd <-0 until initTimes.length) {
            newRow(machInd) = Math.max(lastVal,lastRow(machInd)) + initTimes(machInd)
            lastVal = newRow(machInd)
        }
        newRow
      }
      //calculate each next row and store it in the table
      for(jobInd <- 1 until jobsPermutation.length) {
        val nextRow = calculateNextRow(lastCalculatedRow, jobTimesMatrix(jobsPermutation(jobInd)-1))
        table = Array.concat(table, Array(nextRow))
        lastCalculatedRow = nextRow
      }
      def encapsulate(value:Value,permutation: Array[Int]) = new EvaluatedSolution(value,permutation)
      encapsulate(extractEndTimes(table).max, jobsPermutation)
  }                                               //> evaluatePartialSolution: (jobsPermutation: it.polimi.hyperh.Types.Permutati
                                                  //| on)solution.EvaluatedSolution
  
  //insert the kth job at place,which minimises the partial makespan among the k possible ones
  //first we need to generate all the inserts, e.g.generateInserts(List(1,3),2) produces List(List(2,1,3),List(1,2,3),List(1,3,2))
  def generateInserts(list: List[Int], value: Int):List[List[Int]] = {
    //insert for lists,e.g. insertAt(9999,7,List(1,2,3,4,5,6))
    def insertAt(value:Int, position:Int, list: List[Int]) = {
    if(position<1)
      list
    else if(position>list.size)
      list ::: List(value)
    else
      (list take position-1) ::: List(value) ::: (list drop position-1)
    }
  
    var matrix=List[List[Int]]()
    for(i <-0 to list.size) {
      matrix = matrix ::: List(insertAt(value,i+1,list))
    }
    matrix
  }                                               //> generateInserts: (list: List[Int], value: Int)List[List[Int]]
  
  def loop(listOfTwo: List[Int], remainingJobs:List[Int]):List[Int] = {
    //generates permutations, in this algorithm of two selected elements
    def generatePermutations(jobs: List[Int]):List[List[Int]] = jobs.permutations.toList
    
    //evaluate all permutations, and return the best evaluated solution as pair (value, permutation)
    def getBestPermutation(permutations: List[List[Int]]): (Int,List[Int]) = {
      var evaluatedSolutions = List[EvaluatedSolution]()
      for(i <- 0 until permutations.size) {
        val evaluatedSolution=evaluatePartialSolution(permutations(i).toArray)
        evaluatedSolutions = evaluatedSolutions ::: List(evaluatedSolution)
      }
      val minEvaluatedSolution = evaluatedSolutions.sortBy(_.value).head//endTimes.min
      (minEvaluatedSolution.value, minEvaluatedSolution.solution.toList)
    }
    
    var bestPermutation = getBestPermutation(generatePermutations(listOfTwo))._2
    //STEP 3 of NEH algorithm
    //from 0 until numOfRemainingJobs (in NEH this is marked as for k=3 to numOfJobs)
    for(i <- 0 until remainingJobs.size) {
        bestPermutation = getBestPermutation(generateInserts(bestPermutation,remainingJobs(i)))._2
        //println(bestPermutation)
    }
    bestPermutation
  }                                               //> loop: (listOfTwo: List[Int], remainingJobs: List[Int])List[Int]
  val twoJobs = getWorstNJobs(pairs,2).map(x => x._1).toList
                                                  //> twoJobs  : List[Int] = List(1, 2)
  val remainingJobs = jobs.filterNot(twoJobs.toSet).toList
                                                  //> remainingJobs  : List[Int] = List(3, 4, 5)
  val mySolution = loop(twoJobs, remainingJobs)   //> mySolution  : List[Int] = List(5, 4, 3, 1, 2)
  val myEndTime = evaluatePartialSolution(mySolution.toArray).value
                                                  //> myEndTime  : Int = 58
}