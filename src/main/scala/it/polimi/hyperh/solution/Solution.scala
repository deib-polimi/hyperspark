/**
 *
 */
package it.polimi.hyperh.solution
import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.problem.Problem
import util.SolutionParser
import scala.io.Source

/**
 * @author krle
 *
 */


class Solution (
    val permutation:Permutation
    ) extends Serializable {
    /**
     * A secondary constructor.
     */
    def this(list: List[Int]) {
      this(list.toArray);
    }
	def evaluate(p:Problem):EvaluatedSolution = {
    Problem.evaluate(p, this)
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
