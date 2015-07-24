/**
 *
 */
package solution
import it.polimi.hyperh.Types._
import it.polimi.hyperh.problem.Problem
import util.SolutionParser
import scala.io.Source

/**
 * @author krle
 *
 */


class Solution (
    val permutation:Permutation
    ){

	def evaluate(p:Problem):EvaluatedSolution = {
    val initEndTimesMatrix = p.jobsInitialTimes()
	  new EvaluatedSolution(p.evaluatePartialSolution(permutation, p.jobTimesMatrix, initEndTimesMatrix).value, permutation)
  }
    override def toString = {
      val permString = permutation.mkString(", ")
      val str = "Solution(permutation:Array(" + permString+"))"
      str
    }
}

object Solution{
  def apply(path:String) = SolutionParser.apply(Source.fromFile(path).getLines().mkString)
}
