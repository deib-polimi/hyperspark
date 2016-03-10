package pfsp.algorithms

import it.polimi.hyperh.algorithms.Algorithm
import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.spark.StoppingCondition
import pfsp.problem.PfsProblem
import pfsp.solution.PfsSolution

/**
  * @author Camilo
  */
class RandomAlgorithm() extends Algorithm {
  override def evaluate(problem: Problem): EvaluatedSolution = {
    val p = problem.asInstanceOf[PfsProblem]
    // Generate a random solution by shuffling the list of jobs
    val randomSolution = random.shuffle(p.jobs.toList);
    p.evaluate(PfsSolution(randomSolution.toList))
  }

  // Generate only one solution no need to stop
  override def evaluate(p: Problem, stopCond: StoppingCondition): EvaluatedSolution = {
    evaluate(p)
  }
}
