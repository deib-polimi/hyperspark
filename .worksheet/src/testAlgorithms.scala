import it.polimi.hyperh.problem.Problem
import util.ProblemParser
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.NEHAlgorithm
import it.polimi.hyperh.algorithms.IGAlgorithm

package it.polimi.hyperh

object testAlgorithms {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(327); 
	val path = "D:/Net downloads/Scala/workspace/Thesis/resources/";System.out.println("""path  : String = """ + $show(path ));$skip(98); 
	
  val problem = Problem(path+"inst_ta001").getOrElse(throw new RuntimeException("ParserError"));System.out.println("""problem  : it.polimi.hyperh.problem.Problem = """ + $show(problem ));$skip(228); 
  //problem.numOfMachines
 	//problem.numOfJobs
  //problem.jobTimesMatrix
  //Get OPTIMAL SOLUTION from sol_ta001
  val optimalSolution = EvaluatedSolution(path+"sol_ta001").getOrElse(throw new RuntimeException("ParserError"));System.out.println("""optimalSolution  : it.polimi.hyperh.solution.EvaluatedSolution = """ + $show(optimalSolution ));$skip(97); 
  //Use NEHAlgorithm to evaluate inst_ta001
  val nehEvSolution = NEHAlgorithm.evaluate(problem);System.out.println("""nehEvSolution  : it.polimi.hyperh.solution.EvaluatedSolution = """ + $show(nehEvSolution ));$skip(102); 
  //Use IGAlgorithm to evaluate inst_ta001
  val igEvSolution = IGAlgorithm.evaluate(problem, 2, 0.2);System.out.println("""igEvSolution  : it.polimi.hyperh.solution.EvaluatedSolution = """ + $show(igEvSolution ))}
 

}
