package it.polimi.hyperh.spark

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution

/**
 * @author Nemanja
 */
trait Algorithm_Spark extends Serializable {
  def evaluate(p: Problem): EvaluatedSolution
}