package it.polimi.hyperh.solution

import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.problem.Problem
import util.EvaluatedSolutionParser
import scala.io.Source
import java.io.InputStream

case class EvaluatedSolution(
  val value: Int,
  solution: Permutation)
    extends Solution(solution) with Ordered[EvaluatedSolution] {

  override def evaluate(problem: Problem): EvaluatedSolution = this
  override def toString = {
    val permString = solution.mkString(", ")
    val str = "EvaluatedSolution(value:" + value + ", solution:Array(" + permString + "))"
    str
  }
  override def compare(that: EvaluatedSolution) = this.value - that.value
}

object EvaluatedSolution {
  /**
   * @arg path - name of a file
   */
  def apply(path: String) = EvaluatedSolutionParser.apply(Source.fromFile(path).getLines().mkString).getOrElse(throw new RuntimeException("ParserError"))
  /**
   * @arg name - name of a resource in src/main/resources and src/test/resources
   */
  def fromResources(name: String): EvaluatedSolution = {
    val stream: InputStream = getClass.getResourceAsStream("/" + name)
    EvaluatedSolutionParser(Source.fromInputStream(stream).getLines().mkString).getOrElse(throw new RuntimeException("ParserError"))
  }
}
object DummyEvaluatedSolution {
  def apply(problem: Problem) = new EvaluatedSolution(999999999, problem.jobs)
}