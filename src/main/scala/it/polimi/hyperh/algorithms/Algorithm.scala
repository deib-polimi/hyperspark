package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.solution.Solution

/**
 * @author Nemanja
 */
trait Algorithm extends Serializable {
  def evaluate(p: Problem): EvaluatedSolution
  def evaluate(p: Problem, timeLimit: Double): EvaluatedSolution
  def evaluate(p: Problem, seedSol: Option[Solution], timeLimit: Double): EvaluatedSolution
  def name = this.getClass.getSimpleName
}