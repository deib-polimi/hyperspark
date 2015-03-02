package solution

import it.polimi.hyperh.Types._
import it.polimi.hyperh.problem.Problem
import util.EvaluatedSolutionParser
import scala.io.Source


case class EvaluatedSolution 
(
    val value:Int,
    solution: Permutation 
) 
extends Solution(solution) {
  
  override def evaluate(problem:Problem):EvaluatedSolution = this
}

object EvaluatedSolution{
  def apply(path:String) = EvaluatedSolutionParser.apply(Source.fromFile(path).getLines().mkString)
}