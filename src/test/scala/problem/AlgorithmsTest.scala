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
import util.ConsolePrinter
import it.polimi.hyperh.algorithms.TSABAlgorithm
import it.polimi.hyperh.algorithms.TSABAlgorithm

@Test
class AlgorithmsTest extends Assertions {
  @Test def testAlgorithms() {
    val path = "D:/Net downloads/Scala/workspace/Thesis/resources/"
    val problem = Problem(path + "inst_ta002").getOrElse(throw new RuntimeException("ParserError"))
    
    println("numOfJobs: "+problem.numOfJobs)
    println("numOfMachines: "+problem.numOfMachines)
    //ConsolePrinter.print(problem.jobTimesMatrix)
    
    //Get OPTIMAL SOLUTION from sol_ta001
    val optimalSolution = EvaluatedSolution(path + "sol_ta002").getOrElse(throw new RuntimeException("ParserError"))
    println("Optimal: " + optimalSolution)
    /*
    //Use NEHAlgorithm to evaluate inst_ta001
    val nehAlgorithm = new NEHAlgorithm()
    val nehEvSolution = nehAlgorithm.evaluate(problem)
    println("NEH: " + nehEvSolution)
    
    //Use IGAlgorithm to evaluate inst_ta001
    //d:2, T:0.2
    //val algorithm = new IGAlgorithm(2, 0.2)
    val igAlgorithm = new IGAlgorithm()//initialized with defaults
    val igEvSolution = igAlgorithm.evaluate(problem)
    println("IG: " + igEvSolution)
    
     //Use GAAlgorithm to evaluate inst_ta001
    //popSize:30, crossRate:1.0, mutRate: 0.8, mutDecreaseFactor: 0.99, mutResetThreshold: 0.95
    //val  gaAlgorithm = new GAAlgorithm(30, 1.0, 0.8, 0.99, 0.95)
    val gaAlgorithm = new GAAlgorithm()//initialized with defaults
    val gaEvSolution = gaAlgorithm.evaluate(problem)
    println("GA solution " + gaEvSolution)
    
    //Use SAAlgorithm to evaluate inst_ta001
    //SAAlgorithm sets its parameters based on a size of a problem and a delay matrix sum, so it needs problem variable
    val saAlgorithm = new SAAlgorithm(problem)
    val saEvSolution = saAlgorithm.evaluate(problem)
    println("SA solution " + saEvSolution)
    
    //Use ISAAlgorithm to evaluate inst_ta001
    val isaAlgorithm = new ISAAlgorithm(problem)
    val isaEvSolution = isaAlgorithm.evaluate(problem)
    println("ISA solution " + isaEvSolution)
    //Use TSAlgorithm to evaluate inst_ta001
    val tsAlgorithm = new TSAlgorithm(7)
    val tsEvSolution = tsAlgorithm.evaluate(problem)
    println("TS solution " + tsEvSolution)
    
    */
    //Use TSABAlgorithm to evaluate inst_ta001
    val tsabAlgorithm = new TSABAlgorithm()
    val tsabEvSolution = tsabAlgorithm.evaluate(problem)
    println("TSAB solution " + tsabEvSolution)
    
    assert(true)
    
    
  }

}