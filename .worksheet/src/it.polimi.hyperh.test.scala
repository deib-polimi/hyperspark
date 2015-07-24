package it.polimi.hyperh
import solution._
import it.polimi.hyperh.Types._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(110); 

	val numOfJobs = 5;System.out.println("""numOfJobs  : Int = """ + $show(numOfJobs ));$skip(23); 
	val numOfMachines = 5;System.out.println("""numOfMachines  : Int = """ + $show(numOfMachines ));$skip(119); 
	
	def jobTimesMatrix = Array(Array(5,9,8,10,1),Array(9,3,10,1,8),Array(9,4,5,8,6),Array(4,8,8,7,2),Array(3,5,6,3,7));System.out.println("""jobTimesMatrix: => Array[Array[Int]]""");$skip(40); 
	
  val jobs = (1 to numOfJobs) toArray;System.out.println("""jobs  : Array[Int] = """ + $show(jobs ));$skip(178); 
  
	//associate sequentially job key to array of times present in matrix
  def associateJobToArray(jobs: Array[Int], jobTimesMatrix: Array[Array[Int]]) = jobs zip jobTimesMatrix;System.out.println("""associateJobToArray: (jobs: Array[Int], jobTimesMatrix: Array[Array[Int]])Array[(Int, Array[Int])]""");$skip(761); 
  //associateJobToArray(jobs, jobTimesMatrix)

	//calculate end (completion) times for all jobs on each machine
  def jobsInitialTimes(): Array[Array[Int]] = {
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
    for (i <- 0 until jobTimesMatrix.size)
      resultingMatrix(i) = calculateInitEndTimes(jobTimesMatrix(i))
    resultingMatrix
  };System.out.println("""jobsInitialTimes: ()Array[Array[Int]]""");$skip(298); 
  //GLOBAL VARIABLES, //calculated only during initialization of the algorithm
  //global matrix of end times
  val initEndTimesMatrix = jobsInitialTimes();System.out.println("""initEndTimesMatrix  : Array[Array[Int]] = """ + $show(initEndTimesMatrix ));$skip(276);      // Array(Array(5, 14, 22, 32, 33), Array(9, 12, 22, 23, 31), Array(9, 13, 18, 26, 32), Array(4, 12, 20, 27, 29), Array(3, 8, 14, 17, 24))
  //associate job to an array of its init end times
  val jobTimesPairs = associateJobToArray(jobs, initEndTimesMatrix);System.out.println("""jobTimesPairs  : Array[(Int, Array[Int])] = """ + $show(jobTimesPairs ));$skip(201); //Array((1,Array(5, 14, 22, 32, 33)), (2,Array(9, 12, 22, 23, 31)), (3,Array(9, 13, 18, 26, 32)), (4,Array(4, 12, 20, 27, 29)), (5,Array(3, 8, 14, 17, 24)))
  
  //extracts end times from initEndTimesMatrix, e.g. Array(33, 31, 32, 29, 24)
  def extractEndTimes(matrix: Array[Array[Int]]): Array[Int] = {
    matrix map (array => array(array.length - 1))
  };System.out.println("""extractEndTimes: (matrix: Array[Array[Int]])Array[Int]""");$skip(267); 
  //extractEndTimes(initEndTimesMatrix)           //Array(33, 31, 32, 29, 24)
  //associates job key to value of its end time.e.g. Array((1,33), (2,31), (3,32), (4,29), (5,24))
  def createJobValuePairs(jobs: Array[Int], times: Array[Int]) = {
    jobs zip times
  };System.out.println("""createJobValuePairs: (jobs: Array[Int], times: Array[Int])Array[(Int, Int)]""");$skip(130); 
  //associate job to value of its last init end time
  val pairs = createJobValuePairs(jobs, extractEndTimes(initEndTimesMatrix));System.out.println("""pairs  : Array[(Int, Int)] = """ + $show(pairs ));$skip(192); 
  
  //returns array of times when provided the job value (key)
  def getTimesArrayByJobKey(job: Int, pairs: Array[(Int, Array[Int])]) = pairs.filter(_._1 == job).map(pair => pair._2).flatten;System.out.println("""getTimesArrayByJobKey: (job: Int, pairs: Array[(Int, Array[Int])])Array[Int]""");$skip(42); val res$0 = 
  getTimesArrayByJobKey(1, jobTimesPairs);System.out.println("""res0: Array[Int] = """ + $show(res$0));$skip(175); 
  
  //sort pairs of (jobkey,endTime) by second parameter, in a decreasing order
  def sortJobsDecreasing(pairs: Array[(Int, Int)]) = {
      pairs.sortBy(_._2).reverse
    };System.out.println("""sortJobsDecreasing: (pairs: Array[(Int, Int)])Array[(Int, Int)]""");$skip(211); 
  //gets first N jobs from a sorted (in decreased order) list of pairs (jobkey,endTime)
  def getWorstNJobs(pairs: Array[(Int, Int)], n: Int) = {
    val sorted = sortJobsDecreasing(pairs)
    pairs.take(n)
  };System.out.println("""getWorstNJobs: (pairs: Array[(Int, Int)], n: Int)Array[(Int, Int)]""");$skip(1279); 
  
  
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
  };System.out.println("""evaluatePartialSolution: (jobsPermutation: it.polimi.hyperh.Types.Permutation)solution.EvaluatedSolution""");$skip(789); 
  //evaluatePartialSolution(Array(1,2,3,4,5))
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
  };System.out.println("""generateInserts: (list: List[Int], value: Int)List[List[Int]]""");$skip(216); 
  //generateInserts(List(1,3),2)                  //List(List(2, 1, 3), List(1, 2, 3), List(1, 3, 2))
  //generates permutations
  def generatePermutations(list: List[Int]):List[List[Int]] = list.permutations.toList;System.out.println("""generatePermutations: (list: List[Int])List[List[Int]]""");$skip(620); 
  //generatePermutations(List(1,3))                 //List(List(1, 3), List(3, 1))
  //evaluate all permutations, and return the best evaluated solution as pair (value, permutation)
	def getBestPermutation(permutations: List[List[Int]]): EvaluatedSolution = {
	  var evaluatedSolutions = List[EvaluatedSolution]()
	  for(i <- 0 until permutations.size) {
	    val evaluatedSolution=evaluatePartialSolution(permutations(i).toArray)
	    evaluatedSolutions = evaluatedSolutions ::: List(evaluatedSolution)
	  }
	  val minEvaluatedSolution = evaluatedSolutions.sortBy(_.value).head//endTimes.min
	  minEvaluatedSolution
	};System.out.println("""getBestPermutation: (permutations: List[List[Int]])solution.EvaluatedSolution""");$skip(337); 
  //getBestPermutation(List(List(1, 3), List(3, 1)))//EvaluatedSolution(value:42, solution:Array(3, 1))
	def evaluate(solution: Solution):EvaluatedSolution = {
	  val jobsArray = solution.permutation                      //Array[Int] e.g. 1,2,3,4,5
    val evaluatedSolution = evaluatePartialSolution(jobsArray)
    evaluatedSolution
	};System.out.println("""evaluate: (solution: solution.Solution)solution.EvaluatedSolution""")}
	
}
