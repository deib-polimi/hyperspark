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
import it.polimi.hyperh.algorithms.MMASAlgorithm
import it.polimi.hyperh.algorithms.MMMASAlgorithm
import it.polimi.hyperh.algorithms.PACOAlgorithm
import util.Performance
import it.polimi.hyperh.algorithms.HGAAlgorithm

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
    
    //Use NEHAlgorithm to evaluate inst_ta001
    val nehAlgorithm = new NEHAlgorithm()
    val nehEvSolution = nehAlgorithm.evaluate(problem)
    val nehRPD = Performance.RPD(nehEvSolution, optimalSolution)
    println("NEH: " + nehRPD)
    
    //Use IGAlgorithm to evaluate inst_ta001
    //d:2, T:0.2
    //val algorithm = new IGAlgorithm(2, 0.2)
    val igAlgorithm = new IGAlgorithm()//initialized with defaults
    val igEvSolution = igAlgorithm.evaluate(problem)
    val igRPD = Performance.RPD(igEvSolution, optimalSolution)
    println("IG: " + igRPD)
    
     //Use GAAlgorithm to evaluate inst_ta001
    //popSize:30, crossRate:1.0, mutRate: 0.8, mutDecreaseFactor: 0.99, mutResetThreshold: 0.95
    //val  gaAlgorithm = new GAAlgorithm(30, 1.0, 0.8, 0.99, 0.95)
    val gaAlgorithm = new GAAlgorithm()//initialized with defaults
    val gaEvSolution = gaAlgorithm.evaluate(problem)
    val gaRPD = Performance.RPD(gaEvSolution, optimalSolution)
    println("GA: " + gaRPD)
    
    //Use SAAlgorithm to evaluate inst_ta001
    //SAAlgorithm sets its parameters based on a size of a problem and a delay matrix sum, so it needs problem variable
    val saAlgorithm = new SAAlgorithm(problem)
    val saEvSolution = saAlgorithm.evaluate(problem)
    val saRPD = Performance.RPD(saEvSolution, optimalSolution)
    println("SA: " + saRPD)
    
    //Use ISAAlgorithm to evaluate inst_ta001
    val isaAlgorithm = new ISAAlgorithm(problem)
    val isaEvSolution = isaAlgorithm.evaluate(problem)
    val isaRPD = Performance.RPD(isaEvSolution, optimalSolution)
    println("ISA: " + isaRPD)
    
    //Use TSAlgorithm to evaluate inst_ta001
    val tsAlgorithm = new TSAlgorithm(7)
    val tsEvSolution = tsAlgorithm.evaluate(problem)
    val tsRPD = Performance.RPD(tsEvSolution, optimalSolution)
    println("TS: " + tsRPD)
    
    
    //Use TSABAlgorithm to evaluate inst_ta001
    val tsabAlgorithm = new TSABAlgorithm()
    val tsabEvSolution = tsabAlgorithm.evaluate(problem)
    val tsabRPD = Performance.RPD(tsabEvSolution, optimalSolution)
    println("TSAB: " + tsabRPD)
    
    //Use MMASAlgorithm to evaluate inst_ta001
    val mmasAlgorithm = new MMASAlgorithm(problem)
    val mmasEvSolution = mmasAlgorithm.evaluate(problem)
    val mmasRPD = Performance.RPD(mmasEvSolution, optimalSolution)
    println("MMAS: " + mmasRPD)
    
    //Use MMMASAlgorithm to evaluate inst_ta001
    val mmmasAlgorithm = new MMMASAlgorithm(problem)
    val mmmasEvSolution = mmmasAlgorithm.evaluate(problem)
    val mmmasRPD = Performance.RPD(mmmasEvSolution, optimalSolution)
    println("MMMAS: " + mmmasRPD)
    
    //Use PACOAlgorithm to evaluate inst_ta001
    val pacoAlgorithm = new PACOAlgorithm(problem)
    val pacoEvSolution = pacoAlgorithm.evaluate(problem)
    val pacoRPD = Performance.RPD(pacoEvSolution, optimalSolution)
    println("PACO: " + pacoRPD)
    
    //Use HGAAlgorithm to evaluate inst_ta001
    val hgaAlgorithm = new HGAAlgorithm(problem)
    val hgaEvSolution = hgaAlgorithm.evaluate(problem)
    val hgaRPD = Performance.RPD(hgaEvSolution, optimalSolution)
    println("HGA: " + hgaRPD)
    
    assert(true)
    
    
  }

}