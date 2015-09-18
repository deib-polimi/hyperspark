package it.polimi.hyperh.worksheets
import scala.util.Random
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution

object testFramework {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val evsol = new EvaluatedSolution(9,Array(1,2,4)).asInstanceOf[Solution]
                                                  //> evsol  : it.polimi.hyperh.solution.Solution = EvaluatedSolution(value:9, sol
                                                  //| ution:Array(1, 2, 4))
  val sol = new Solution(Array(1,2,4))            //> sol  : it.polimi.hyperh.solution.Solution = Solution(permutation:Array(1, 2,
                                                  //|  4))

  evsol.isInstanceOf[EvaluatedSolution]           //> res0: Boolean = true
  sol.isInstanceOf[EvaluatedSolution]             //> res1: Boolean = false
  
  def isSubclass(instance: Solution) = {
  	instance.isInstanceOf[EvaluatedSolution]
  }                                               //> isSubclass: (instance: it.polimi.hyperh.solution.Solution)Boolean
  isSubclass(evsol)                               //> res2: Boolean = true
  isSubclass(sol)                                 //> res3: Boolean = false
  if(isSubclass(evsol))
  	evsol.asInstanceOf[EvaluatedSolution]     //> res4: Any = EvaluatedSolution(value:9, solution:Array(1, 2, 4))
}