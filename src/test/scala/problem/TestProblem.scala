package problem

import org.scalatest.Assertions
import org.junit.Test
import it.polimi.hyperh.problem.Problem
import solution.Solution
import solution.EvaluatedSolution
import it.polimi.hyperh.problem.IGAlgorithm

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
@Test def testIgOnTaillardInstances() {
  
  assert(true)
}
////////////////////////////////////////////////////
@Test def testEvalSyntetic() {
	val problem = new Problem(2,3,Array(Array(1,3,5),Array(3,2,4)))
	val solution = new Solution(List(1,2,0).toArray)
	val evalSolution = solution.evaluate(problem)   
	assert(evalSolution.value===15)
}

}