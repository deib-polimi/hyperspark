package util
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution

/**
 * @author Nemanja
 */

object PermutationUtility {
  //generates permutations
  def generatePermutations(list: List[Int]):List[List[Int]] = list.permutations.toList
  
  //generate all the inserts, e.g.generateInserts(List(1,3),2) produces List(List(2,1,3),List(1,2,3),List(1,3,2))
  def generateInserts(list: List[Int], value: Int):List[List[Int]] = {
    //insert for lists,e.g. insertAt(9999,7,List(1,2,3,4,5,6))
    def insertAt(value:Int, position:Int, list: List[Int]) = {
    if(position<1)
      list
    else if(position>list.size)
      list ::: List(value)
    else
      (list take position-1) ::: List(value) ::: (list drop position-1)
    }                                         //> insertAt: (value: Int, position: Int, list: List[Int])List[Int]
  
    var matrix=List[List[Int]]()
    for(i <-0 to list.size) {
      matrix = matrix ::: List(insertAt(value,i+1,list))
    }
    matrix
  }  
  
  //evaluate all permutations, and return the best evaluated solution as pair (value, permutation)
    def getBestPermutation(permutations: List[List[Int]],p:Problem): EvaluatedSolution = {
      var evaluatedSolutions = List[EvaluatedSolution]()
      for(i <- 0 until permutations.size) {
        val evaluatedSolution=p.evaluatePartialSolution(permutations(i))
        evaluatedSolutions = evaluatedSolutions ::: List(evaluatedSolution)
      }
      val minEvaluatedSolution = evaluatedSolutions.sortBy(_.value).head//endTimes.min
      minEvaluatedSolution
    }
}