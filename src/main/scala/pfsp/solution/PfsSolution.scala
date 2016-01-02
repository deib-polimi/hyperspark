package pfsp.solution

import it.polimi.hyperh.solution.Solution
import pfsp.util.PfsSolutionParser
import scala.io.Source


/**
 * @author Nemanja
 */
class PfsSolution(val permutation:Array[Int]) extends Solution {
  def asString() = "Array(" + permutation.mkString(", ")+")"
  override def toString = {
    val permString = asString()
    val str = "PfsSolution(permutation:" + permString+")"
    str
  }
  def toList = permutation.toList
}
object PfsSolution{
  def fromFile(path:String) = PfsSolutionParser.apply(Source.fromFile(path).getLines().mkString).getOrElse(throw new RuntimeException("ParserError"))
  def apply(permutation: Array[Int]) = new PfsSolution(permutation)
  def apply(permutation: List[Int]) = new PfsSolution(permutation.toArray)
}