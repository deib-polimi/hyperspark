package it.polimi.hyperh.solution

import it.polimi.hyperh.types.Types._
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
  override def toString = {
      val permString = solution.mkString(", ")
      val str = "EvaluatedSolution(value:"+ value + ", solution:Array(" + permString+"))"
      str
    }
}

object EvaluatedSolution{
  def apply(path:String) = EvaluatedSolutionParser.apply(Source.fromFile(path).getLines().mkString)
}