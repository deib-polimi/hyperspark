/**
 *
 */
package it.polimi.hyperh.solution
import it.polimi.hyperh.types.Types._
import it.polimi.hyperh.problem.Problem
import scala.io.Source

abstract class Solution extends Serializable {
  
	def evaluate(p:Problem):EvaluatedSolution = {
    p.evaluate(this)
  }
  override def toString = "abstract solution"
}

