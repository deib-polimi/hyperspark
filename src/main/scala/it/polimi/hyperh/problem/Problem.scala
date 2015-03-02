package it.polimi.hyperh.problem

import Array._
import scala.io.Source
import it.polimi.hyperh._
import it.polimi.hyperh.Types._
import util.ProblemParser
import solution.Solution
import solution.EvaluatedSolution



@SerialVersionUID(100L)
class Problem  (
	n:Int,					//num of machines
	m:Int,					//num of jobs
	d: Array[Array[Int]] 	// delays(i)(j) of j-th job on i-th machine (0<=i<n) (0<=j<m)
) extends Serializable
{
	def jobs = m
	def machines = n
	def delays = d
	
	val np = n-1		//num of machines-1
	val mp = m-1		//num of jobs-1
	def evaluateSolution(solution: Solution):EvaluatedSolution = {
	  val jobs = solution.permutation
	  def encapsulate(value:Value) = new EvaluatedSolution(value,jobs)
	  def iterEval(machine:Int,job:Int, value:Value):Value = {
	    (machine,job) match {
	    case (this.np, this.mp) => value
	    case (y		 , this.mp) => iterEval(y+1,mp, value+delays(y+1)(jobs(this.mp)))
	    case (this.np, x) 		=> iterEval(np,x+1, value+delays(this.np)(jobs(x+1)))
	    case (y		 , x) 		=> if(delays(y)(jobs(x+1))>delays(y+1)(jobs(x)))
	    								iterEval(y,x+1, value+delays(y)(jobs(x+1)))
	    						   else
	    							   	iterEval(y+1,x, value+delays(y+1)(jobs(x)))
	    }
	  }
	  encapsulate(iterEval(0, 0, delays(0)(jobs(0))))
	}
}

//Problem Factory
object Problem{
	/**
	 * @arg path - path to a file 
	 */	
	def apply(path:String) = ProblemParser.apply(Source.fromFile(path).getLines().mkString(" x ") + " x ")

}