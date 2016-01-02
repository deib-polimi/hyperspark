package pfsp.solution

import it.polimi.hyperh.solution.EvaluatedSolution
import pfsp.problem.PfsProblem
import pfsp.util.PfsEvaluatedSolutionParser
import scala.io.Source
import java.io.InputStream

/**
 * @author Nemanja
 */
class PfsEvaluatedSolution(override val value: Int, override val solution: PfsSolution) extends EvaluatedSolution(value, solution)
{
  //Alternative constructor
  def this(value: Int, permutation: Array[Int]) = this(value, PfsSolution(permutation))
  override def toString = {
    val permString = solution.asString()
    val str = "PfsEvaluatedSolution(value:" + value + ", solution:" + permString + ")"
    str
  }
  def compare(that: EvaluatedSolution) = this.value - that.asInstanceOf[PfsEvaluatedSolution].value
  def compare(that: PfsEvaluatedSolution) = this.value - that.value
  def permutation = solution.permutation
}
object PfsEvaluatedSolution {
  /**
   * @arg path - name of a file
   */
  def fromFile(path: String) = PfsEvaluatedSolutionParser.apply(Source.fromFile(path).getLines().mkString).getOrElse(throw new RuntimeException("ParserError"))
  /**
   * @arg name - name of a resource in src/main/resources and src/test/resources
   */
  def fromResources(name: String) = {
    val stream: InputStream = getClass.getResourceAsStream("/" + name)
    PfsEvaluatedSolutionParser(Source.fromInputStream(stream).getLines().mkString).getOrElse(throw new RuntimeException("ParserError"))
  }
}
object BadPfsEvaluatedSolution {
  def apply(problem: PfsProblem) = new PfsEvaluatedSolution(999999999, problem.jobs)
}