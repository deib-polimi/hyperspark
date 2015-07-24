package it.polimi.hyperh
import solution._
import it.polimi.hyperh.Types._

object testNEHproblem {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(119); 
		val numOfJobs = 5;System.out.println("""numOfJobs  : Int = """ + $show(numOfJobs ));$skip(23); 
	val numOfMachines = 5;System.out.println("""numOfMachines  : Int = """ + $show(numOfMachines ));$skip(118); 
	
	def jobTimesMatrix = Array(Array(5,9,8,10,1),Array(9,3,10,1,8),Array(9,4,5,8,6),Array(4,8,8,7,2),Array(3,5,6,3,7));System.out.println("""jobTimesMatrix: => Array[Array[Int]]""");$skip(177); 
	
	//associate sequentially job key to array of times present in matrix
  def associateJobToArray(jobs: Array[Int], jobTimesMatrix: Array[Array[Int]]) = jobs zip jobTimesMatrix;System.out.println("""associateJobToArray: (jobs: Array[Int], jobTimesMatrix: Array[Array[Int]])Array[(Int, Array[Int])]""");$skip(189); 
  //returns array of times when provided the job value (key)
  def getTimesArrayByJobKey(job: Int, pairs: Array[(Int, Array[Int])]) = pairs.filter(_._1 == job).map(pair => pair._2).flatten;System.out.println("""getTimesArrayByJobKey: (job: Int, pairs: Array[(Int, Array[Int])])Array[Int]""");$skip(761); 

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
  };System.out.println("""jobsInitialTimes: (matrix: Array[Array[Int]], numOfJobs: Int, numOfMachines: Int)Array[Array[Int]]""");$skip(198); 
  //extracts end times from initEndTimesMatrix, e.g. Array(33, 31, 32, 29, 24)
  def extractEndTimes(matrix: Array[Array[Int]]): Array[Int] = {
    matrix map (array => array(array.length - 1))
  };System.out.println("""extractEndTimes: (matrix: Array[Array[Int]])Array[Int]""");$skip(189); 
  //associates job key to value of its end time.e.g. Array((1,33), (2,31), (3,32), (4,29), (5,24))
  def createJobValuePairs(jobs: Array[Int], times: Array[Int]) = {
    jobs zip times
  };System.out.println("""createJobValuePairs: (jobs: Array[Int], times: Array[Int])Array[(Int, Int)]""");$skip(387); 
  //gets first N jobs from a sorted (in decreased order) list of pairs (jobkey,endTime)
  def getWorstNJobs(pairs: Array[(Int, Int)], n: Int) = {
    //sort pairs of (jobkey,endTime) by second parameter, in a decreasing order
    def sortJobsDecreasing(pairs: Array[(Int, Int)]) = {
      pairs.sortBy(_._2).reverse
    }
    val sorted = sortJobsDecreasing(pairs)
    pairs.take(n)
  };System.out.println("""getWorstNJobs: (pairs: Array[(Int, Int)], n: Int)Array[(Int, Int)]""");$skip(196); 
  //GLOBAL VARIABLES, //calculated only during initialization of the algorithm
  //global matrix of end times
  val initEndTimesMatrix = jobsInitialTimes(jobTimesMatrix, numOfJobs, numOfMachines);System.out.println("""initEndTimesMatrix  : Array[Array[Int]] = """ + $show(initEndTimesMatrix ));$skip(38); 
  val jobs = (1 to numOfJobs) toArray;System.out.println("""jobs  : Array[Int] = """ + $show(jobs ));$skip(130); 
  //associate job to value of its last init end time
  val pairs = createJobValuePairs(jobs, extractEndTimes(initEndTimesMatrix));System.out.println("""pairs  : Array[(Int, Int)] = """ + $show(pairs ));$skip(120); 
  //associate job to an array of its init end times
  val jobTimesPairs = associateJobToArray(jobs, initEndTimesMatrix);System.out.println("""jobTimesPairs  : Array[(Int, Array[Int])] = """ + $show(jobTimesPairs ));$skip(1276); 
  
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
  };System.out.println("""evaluatePartialSolution: (jobsPermutation: it.polimi.hyperh.Types.Permutation)solution.EvaluatedSolution""");$skip(746); 
  
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
  };System.out.println("""generateInserts: (list: List[Int], value: Int)List[List[Int]]""");$skip(1261); 
  
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
  };System.out.println("""loop: (listOfTwo: List[Int], remainingJobs: List[Int])List[Int]""");$skip(61); 
  val twoJobs = getWorstNJobs(pairs,2).map(x => x._1).toList;System.out.println("""twoJobs  : List[Int] = """ + $show(twoJobs ));$skip(59); 
  val remainingJobs = jobs.filterNot(twoJobs.toSet).toList;System.out.println("""remainingJobs  : List[Int] = """ + $show(remainingJobs ));$skip(48); 
  val mySolution = loop(twoJobs, remainingJobs);System.out.println("""mySolution  : List[Int] = """ + $show(mySolution ));$skip(68); 
  val myEndTime = evaluatePartialSolution(mySolution.toArray).value;System.out.println("""myEndTime  : Int = """ + $show(myEndTime ))}
}
