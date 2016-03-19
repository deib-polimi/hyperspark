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
  private var canModifyRandom = true
  def evaluate(p: Problem): EvaluatedSolution
  def evaluate(p: Problem, stopCond: StoppingCondition): EvaluatedSolution
  def evaluate(p: Problem, seedSol: Option[Solution], stopCond: StoppingCondition): EvaluatedSolution = {
    seed = seedSol
    evaluate(p, stopCond)
  }
  def evaluate(p: Problem, seedSol: Option[Solution], stopCond: StoppingCondition, runNo: Int): EvaluatedSolution = {
    seed = seedSol
    this.runNo = runNo
    if (canModifyRandom) {
      random = new Random(runNo)
    }
    val solution = evaluate(p, stopCond)
    solution
  }
  def name = this.getClass.getSimpleName
  def setRandomSeed(seed: Long) = {
    random = new Random(seed)
    canModifyRandom = false
    this
  }
}