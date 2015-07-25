package it.polimi.hyperh
import scala.util.Random
import it.polimi.hyperh.types.Types._
import solution._
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.problem._
import util.ProblemParser
import it.polimi.hyperh.algorithms._
import scala.io.Source

object testAlgorithms {
	val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"

  val problem = Problem(path+"inst_ta001").getOrElse(throw new RuntimeException("ParserError"))
  //problem.numOfMachines
 	//problem.numOfJobs
  //problem.jobTimesMatrix
  //Get OPTIMAL SOLUTION from sol_ta001
  val optimalSolution = EvaluatedSolution(path+"sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
  //Use NEHAlgorithm to evaluate inst_ta001
  val nehEvSolution = NEHAlgorithm.evaluate(problem)
  //Use IGAlgorithm to evaluate inst_ta001
  val igEvSolution = IGAlgorithm.evaluate(problem, 2, 0.2)
 

}