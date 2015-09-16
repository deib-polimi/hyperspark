package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution

/**
 * @author Nemanja
 */
trait Algorithm {
  def evaluate(p: Problem): EvaluatedSolution
  def evaluate(p: Problem, timeLimit: Double): EvaluatedSolution
}