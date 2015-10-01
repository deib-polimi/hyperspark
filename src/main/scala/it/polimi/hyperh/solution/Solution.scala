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
    def this(permutation: List[Int]) {
      this(permutation.toArray);
    }
	def evaluate(p:Problem):EvaluatedSolution = {
    p.evaluate(this)
  }
    override def toString = {
      val permString = permutation.mkString(", ")
      val str = "Solution(permutation:Array(" + permString+"))"
      str
    }
}

object Solution{
  def apply(path:String) = SolutionParser.apply(Source.fromFile(path).getLines().mkString).getOrElse(throw new RuntimeException("ParserError"))
  def apply(permutation:Permutation) = new Solution(permutation)
  def apply(permutation: List[Int]) = new Solution(permutation)
}
