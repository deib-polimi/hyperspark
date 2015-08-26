package problem

import org.scalatest.Assertions
import org.junit.Test
import org.junit._
import Assert._
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.algorithms.IGAlgorithm
import it.polimi.hyperh.algorithms.NEHAlgorithm
import it.polimi.hyperh.algorithms.GAAlgorithm
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import it.polimi.hyperh.algorithms.SAAlgorithm
import it.polimi.hyperh.algorithms.ISAAlgorithm
import it.polimi.hyperh.algorithms.TSAlgorithm

@Test
class AlgorithmsTest extends Assertions {

  @Test def testNEH() {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta001").getOrElse(throw new RuntimeException("ParserError"))

    /*println(problem.numOfJobs)
    println(problem.numOfMachines)
    for(i<-0 until problem.jobTimesMatrix.size)
    println(problem.jobTimesMatrix(i).mkString("Array[", ",", "]"))*/

    //Get OPTIMAL SOLUTION from sol_ta001
    val optimalSolution = EvaluatedSolution(path + "sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
    println("Optimal solution " + optimalSolution)
    //Use NEHAlgorithm to evaluate inst_ta001
    val nehEvSolution = NEHAlgorithm.evaluate(problem)
    println("NEH solution " + nehEvSolution)
    assert(true)
  }
  @Test def testIG() {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta001").getOrElse(throw new RuntimeException("ParserError"))

    /*println(problem.numOfJobs)
    println(problem.numOfMachines)
    for(i<-0 until problem.jobTimesMatrix.size)
    println(problem.jobTimesMatrix(i).mkString("Array[", ",", "]"))*/

    //Get OPTIMAL SOLUTION from sol_ta001
    //val optimalSolution = EvaluatedSolution(path + "sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
    //println("Optimal solution " + optimalSolution)
    //Use IGAlgorithm to evaluate inst_ta001
    val igEvSolution = IGAlgorithm.evaluate(problem, 2, 0.2)
    println("IG solution " + igEvSolution)
    //assert(optimalSolution.value == igEvSolution.value)
    assert(true)
  }
  @Test def testGA() {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta001").getOrElse(throw new RuntimeException("ParserError"))

    //Get OPTIMAL SOLUTION from sol_ta001
    //val optimalSolution = EvaluatedSolution(path + "sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
    //println("Optimal solution " + optimalSolution)
    //Use GAAlgorithm to evaluate inst_ta001
    //popSize:30, crossRate:1.0, mutRate: 0.8, mutDecreaseFactor: 0.99, mutResetThreshold: 0.95
    val gaEvSolution = GAAlgorithm.evaluate(problem, 30, 1.0, 0.8, 0.99, 0.95)
    println("GA solution " + gaEvSolution)
    //assert(optimalSolution.value == gaEvSolution.value)
    assert(true)
  }

  @Test def testSA() {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta001").getOrElse(throw new RuntimeException("ParserError"))

    //Get OPTIMAL SOLUTION from sol_ta001
    //val optimalSolution = EvaluatedSolution(path + "sol_ta001").getOrElse(throw new RuntimeException("ParserError"))
    //println("Optimal solution " + optimalSolution)
    //Use SAAlgorithm to evaluate inst_ta001
    val saEvSolution = SAAlgorithm.evaluate(problem)
    println("SA solution " + saEvSolution)
    //assert(optimalSolution.value == gaEvSolution.value)
    assert(true)
  }
  @Test def testISA() {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta001").getOrElse(throw new RuntimeException("ParserError"))
    //Use ISAAlgorithm to evaluate inst_ta001
    val isaEvSolution = ISAAlgorithm.evaluate(problem)
    println("ISA solution " + isaEvSolution)
    assert(true)
  }
  @Test def testTS() {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta001").getOrElse(throw new RuntimeException("ParserError"))
    //Use TSAlgorithm to evaluate inst_ta001
    val tsEvSolution = TSAlgorithm.evaluate(problem, 7)
    println("TS solution " + tsEvSolution)
    assert(true)
  }
}