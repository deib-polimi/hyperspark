package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.solution.Solution
import scala.util.Random

/**
 * @author Nemanja
 */
trait Algorithm extends Serializable {
  protected var seed: Option[Solution] = None
  protected var run: Int = 1
  protected var random: Random = new Random(run)
  def evaluate(p: Problem): EvaluatedSolution
  def evaluate(p: Problem, timeLimit: Double): EvaluatedSolution
  def evaluate(p: Problem, seedSol: Option[Solution], timeLimit: Double): EvaluatedSolution = {
    seed = seedSol
    evaluate(p, timeLimit)
  }
  def evaluate(p: Problem, seedSol: Option[Solution], timeLimit: Double, run: Int): EvaluatedSolution = {
    seed = seedSol
    this.run = run
    random = new Random(run)
    evaluate(p, timeLimit)
  }
  def name = this.getClass.getSimpleName
}