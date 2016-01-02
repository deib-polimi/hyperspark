package it.polimi.hyperh.problem

import it.polimi.hyperh._
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution

@SerialVersionUID(100L)
abstract class Problem() extends Serializable
{
  def evaluate(s: Solution): EvaluatedSolution
}