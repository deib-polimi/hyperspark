package it.polimi.hyperh.solution

import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.problem.Problem

abstract class EvaluatedSolution(
  val value: AnyVal,
  val solution: Solution)
    extends Solution with Ordered[EvaluatedSolution] {
    
  override def evaluate(problem: Problem): EvaluatedSolution = this
  override def toString = {
    val solString = solution.toString()
    val str = "EvaluatedSolution(value:" + value + ", solution:" + solString + ")"
    str
  }
}
