package solution

import it.polimi.hyperh.Types._
import it.polimi.hyperh.problem.Problem
import util.SolutionParser
import scala.io.Source


case class PartialSolution (
	constraints:Constraints,
    solution: Permutation
) extends Solution(solution) {
	//def this(solution: Permutation) = this(new Array((x:Int)=>true),solution)
}