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

	def evaluate(problem:Problem):EvaluatedSolution = 
	  problem.eval(this)
}

object Solution{
  def apply(path:String) = SolutionParser.apply(Source.fromFile(path).getLines().mkString)
}
