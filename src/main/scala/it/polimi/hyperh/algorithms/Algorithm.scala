package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.solution.Solution
import util.Random
import it.polimi.hyperh.spark.StoppingCondition

/**
 * @author Nemanja
 */
trait Algorithm extends Serializable {
  protected var seed: Option[Solution] = None
  protected var runNo: Int = 1
  protected var random: Random = new Random(runNo)
  def evaluate(p: Problem): EvaluatedSolution
  def evaluate(p: Problem, stopCond: StoppingCondition): EvaluatedSolution
  def evaluate(p: Problem, seedSol: Option[Solution], stopCond: StoppingCondition): EvaluatedSolution = {
    seed = seedSol
    evaluate(p, stopCond)
  }
  def evaluate(p: Problem, seedSol: Option[Solution], stopCond: StoppingCondition, runNo: Int): EvaluatedSolution = {
    seed = seedSol
    this.runNo = runNo
    random = new Random(runNo)
    evaluate(p, stopCond)
  }
  def name = this.getClass.getSimpleName
}