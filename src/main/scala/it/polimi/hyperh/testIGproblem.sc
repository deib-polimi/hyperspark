package it.polimi.hyperh
import scala.util.Random
import it.polimi.hyperh.Types._
import solution._
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.problem._
import util.ProblemParser
import scala.io.Source

object testIGproblem {
	val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
                                                  //> path  : String = D:/Net downloads/Scala/workspace/Thesis/resources/

  val problem = Problem(path+"inst_ta001").getOrElse(throw new RuntimeException("ParserError"))
                                                  //> problem  : it.polimi.hyperh.problem.Problem = it.polimi.hyperh.problem.Probl
                                                  //| em@34c4973
  //problem.numOfMachines
 	//problem.numOfJobs
  //problem.jobTimesMatrix
  //Get OPTIMAL SOLUTION from sol_ta001
  val optimalSolution = EvaluatedSolution(path+"sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
                                                  //> optimalSolution  : solution.EvaluatedSolution = EvaluatedSolution(value:1278
                                                  //| , solution:Array(3, 17, 15, 8, 9, 6, 5, 14, 16, 7, 11, 13, 18, 19, 1, 4, 2, 
                                                  //| 10, 20, 12))
  //Use NEHAlgorithm to evaluate inst_ta001
  val nehEvSolution = NEHAlgorithm.evaluate(problem)
                                                  //> nehEvSolution  : solution.EvaluatedSolution = EvaluatedSolution(value:1310, 
                                                  //| solution:Array(17, 9, 15, 16, 6, 19, 3, 1, 18, 4, 2, 8, 5, 7, 11, 13, 10, 12
                                                  //| , 14, 20))
  //Use IGAlgorithm to evaluate inst_ta001
  val igEvSolution = IGAlgorithm.evaluate(problem, 2, 0.2)
                                                  //> igEvSolution  : solution.EvaluatedSolution = EvaluatedSolution(value:1278, s
                                                  //| olution:Array(17, 3, 15, 6, 9, 14, 13, 11, 5, 7, 8, 1, 19, 18, 16, 4, 2, 10,
                                                  //|  20, 12))
 

}