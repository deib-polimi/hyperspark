package problem

import org.scalatest.Assertions
import org.junit.Test
import org.junit._
import Assert._
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import pfsp.algorithms.IGAlgorithm
import pfsp.algorithms.NEHAlgorithm
import pfsp.algorithms.GAAlgorithm
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import pfsp.algorithms.SAAlgorithm
import pfsp.algorithms.TSAlgorithm
import util.ConsolePrinter
import pfsp.algorithms.TSABAlgorithm
import pfsp.algorithms.MMASAlgorithm
import util.Performance
import pfsp.algorithms.HGAAlgorithm
import pfsp.problem.PfsProblem
import pfsp.solution.PfsEvaluatedSolution
import it.polimi.hyperh.spark.TimeExpired
import pfsp.algorithms.MMMASAlgorithm
import pfsp.algorithms.PACOAlgorithm
import pfsp.algorithms.ISAAlgorithm

@Test
class AlgorithmsTest extends Assertions {
  @Test def testAlgorithms() {
    //uncomment to use testing
    /*
    val i = "008"  //taillard's PFSP instance number "i"
    val problem = PfsProblem.fromResources("inst_ta"+i+".txt")
    
    println("numOfJobs: "+problem.numOfJobs)
    println("numOfMachines: "+problem.numOfMachines)
   //ConsolePrinter.print(problem.jobTimesMatrix)
    
    val timeLimit =  problem.getExecutionTime()
    val stopCond = new TimeExpired(timeLimit)
    println("Time limit: "+timeLimit/1000.0+"s")
    
    //Get OPTIMAL SOLUTION from sol_ta+"i"
    val bestFoundSol = PfsEvaluatedSolution.fromResources("sol_ta"+i+".txt")
    println("BestFound: " + bestFoundSol)
    
    //Use NEHAlgorithm to evaluate inst_ta+"i"
    val nehAlgorithm = new NEHAlgorithm()
    val nehEvSolution = nehAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val nehRPD = Performance.RPD(nehEvSolution, bestFoundSol)
    println("NEH: " + nehRPD)
    
    //Use IGAlgorithm to evaluate inst_ta+"i"
    //d:2, T:0.2
    //val algorithm = new IGAlgorithm(2, 0.2)
    val igAlgorithm = new IGAlgorithm()//initialized with defaults
    val igEvSolution = igAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val igRPD = Performance.RPD(igEvSolution, bestFoundSol)
    println("IG: " + igRPD)
    
     //Use GAAlgorithm to evaluate inst_ta+"i"
    //popSize:30, crossRate:1.0, mutRate: 0.8, mutDecreaseFactor: 0.99, mutResetThreshold: 0.95
    val gaAlgorithm = new GAAlgorithm()//initialized with defaults
    val gaEvSolution = gaAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val gaRPD = Performance.RPD(gaEvSolution, bestFoundSol)
    println("GA: " + gaRPD)
    
    //Use SAAlgorithm to evaluate inst_ta+"i"
    val saAlgorithm = new SAAlgorithm(problem)
    val saEvSolution = saAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val saRPD = Performance.RPD(saEvSolution, bestFoundSol)
    println("SA: " + saRPD)
    
    //Use ISAAlgorithm to evaluate inst_ta+"i"
    val isaAlgorithm = new ISAAlgorithm(problem)
    val isaEvSolution = isaAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val isaRPD = Performance.RPD(isaEvSolution, bestFoundSol)
    println("ISA: " + isaRPD)
    
    //Use TSAlgorithm to evaluate inst_ta+"i"
    val tsAlgorithm = new TSAlgorithm()
    val tsEvSolution = tsAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val tsRPD = Performance.RPD(tsEvSolution, bestFoundSol)
    println("TS: " + tsRPD)
    
    
    //Use TSABAlgorithm to evaluate inst_ta+"i"
    val tsabAlgorithm = new TSABAlgorithm()
    val tsabEvSolution = tsabAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val tsabRPD = Performance.RPD(tsabEvSolution, bestFoundSol)
    println("TSAB: " + tsabRPD)
    
    //Use MMASAlgorithm to evaluate inst_ta+"i"
    val mmasAlgorithm = new MMASAlgorithm(problem)
    val mmasEvSolution = mmasAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val mmasRPD = Performance.RPD(mmasEvSolution, bestFoundSol)
    println("MMAS: " + mmasRPD)
    
    //Use MMMASAlgorithm to evaluate inst_ta+"i"
    val mmmasAlgorithm = new MMMASAlgorithm(problem)
    val mmmasEvSolution = mmmasAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val mmmasRPD = Performance.RPD(mmmasEvSolution, bestFoundSol)
    println("MMMAS: " + mmmasRPD)
    
    //Use PACOAlgorithm to evaluate inst_ta+"i"
    val pacoAlgorithm = new PACOAlgorithm(problem)
    val pacoEvSolution = pacoAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val pacoRPD = Performance.RPD(pacoEvSolution, bestFoundSol)
    println("PACO: " + pacoRPD)
    
    //Use HGAAlgorithm to evaluate inst_ta+"i"
    val hgaAlgorithm = new HGAAlgorithm(problem)
    val hgaEvSolution = hgaAlgorithm.evaluate(problem, stopCond).asInstanceOf[PfsEvaluatedSolution]
    val hgaRPD = Performance.RPD(hgaEvSolution, bestFoundSol)
    println("HGA: " + hgaRPD)
    */
    assert(true)
    
    
  }

}