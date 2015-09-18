package it.polimi.hyperh.solution

import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.problem.Problem
import util.EvaluatedSolutionParser
import scala.io.Source


case class EvaluatedSolution 
(
    val value:Int,
    solution: Permutation 
) 
extends Solution(solution) with Ordered[EvaluatedSolution] {
  
  override def evaluate(problem:Problem):EvaluatedSolution = this
  override def toString = {
      val permString = solution.mkString(", ")
      val str = "EvaluatedSolution(value:"+ value + ", solution:Array(" + permString+"))"
      str
    }
  override def compare(that: EvaluatedSolution) = this.value - that.value
}

object EvaluatedSolution{
  def apply(path:String) = EvaluatedSolutionParser.apply(Source.fromFile(path).getLines().mkString)
}
object DummyEvaluatedSolution {
  def apply(problem: Problem) = new EvaluatedSolution(999999999, problem.jobs)
}