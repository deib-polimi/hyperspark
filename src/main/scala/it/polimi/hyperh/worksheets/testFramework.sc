package it.polimi.hyperh.worksheets
import scala.util.Random
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution

object testFramework {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
 	def checkDuplicates(list: List[Int]): Boolean = {
 		list.distinct.size != list.size
 	}                                         //> checkDuplicates: (list: List[Int])Boolean
}