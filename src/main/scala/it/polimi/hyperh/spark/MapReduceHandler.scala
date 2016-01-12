package it.polimi.hyperh.spark

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution

/**
 * @author Nemanja
 */
class MapReduceHandler {
  final def hyperMap(problem: Problem, d: DistributedDatum, runNo: Int): EvaluatedSolution = {
    d.algorithm.evaluate(problem, d.seed, d.stoppingCondition, runNo)
  }
  def hyperReduce(sol1: EvaluatedSolution, sol2: EvaluatedSolution): EvaluatedSolution = {
    List(sol1, sol2).min
  }
}