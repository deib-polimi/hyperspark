package problem

import org.scalatest.Assertions
import org.junit.Test
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.IGAlgorithm
import it.polimi.hyperh.algorithms.NEHAlgorithm

class TestProblem extends Assertions {

@Test def testEvalReal() {
  for(i<- 1 to 5){
	val optproblem = Problem("./resources/inst_ta00"+i)
	val problem = optproblem.getOrElse(throw new RuntimeException("ParserError"))   
	
	val optsolution = Solution("./resources/sol_ta00"+i)
	val solution = optsolution.getOrElse(throw new RuntimeException("ParserError"))   
	
	val optevalsolution = EvaluatedSolution("./resources/sol_ta00"+i)
	val evalsolution = optevalsolution.getOrElse(throw new RuntimeException("ParserError"))   
  
	val evalSolution = solution.evaluate(problem)
	assert(evalSolution.value===evalsolution.value)
  }
}

////////////////////////////////////////////////////
@Test def testNEH() {
  val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
  val problem = Problem(path+"inst_ta001").getOrElse(throw new RuntimeException("ParserError"))
  
  /*println(problem.numOfJobs)
  println(problem.numOfMachines)
  for(i<-0 until problem.jobTimesMatrix.size)
    println(problem.jobTimesMatrix(i).mkString("Array[", ",", "]"))*/
  
  //Get OPTIMAL SOLUTION from sol_ta001
  val optimalSolution = EvaluatedSolution(path+"sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
  println("Optimal solution "+optimalSolution)
  //Use NEHAlgorithm to evaluate inst_ta001
  val nehEvSolution = NEHAlgorithm.evaluate(problem)
  println("NEH solution "+nehEvSolution)
  assert(true)
}
@Test def testIG() {
  val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
  val problem = Problem(path+"inst_ta001").getOrElse(throw new RuntimeException("ParserError"))
  
  /*println(problem.numOfJobs)
  println(problem.numOfMachines)
  for(i<-0 until problem.jobTimesMatrix.size)
    println(problem.jobTimesMatrix(i).mkString("Array[", ",", "]"))*/
  
  //Get OPTIMAL SOLUTION from sol_ta001
  val optimalSolution = EvaluatedSolution(path+"sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
  println("Optimal solution "+optimalSolution)
  //Use IGAlgorithm to evaluate inst_ta001
  val igEvSolution = IGAlgorithm.evaluate(problem, 2, 0.2)
  println("IG solution "+igEvSolution)
  assert(optimalSolution.value == igEvSolution.value)
}
////////////////////////////////////////////////////
@Test def testEvalSyntetic() {
	val problem = new Problem(2,3,Array(Array(1,3,5),Array(3,2,4)))
	val solution = new Solution(List(1,2,0).toArray)
	val evalSolution = solution.evaluate(problem)   
	assert(evalSolution.value===15)
}

}