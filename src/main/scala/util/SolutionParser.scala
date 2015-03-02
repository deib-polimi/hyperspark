package util

import scala.util.parsing.combinator.RegexParsers
import solution.Solution
import solution.EvaluatedSolution

object SolutionParser extends RegexParsers {

	def number: Parser[Int] = """\d+""".r ^^ { _.toInt-1 }
	def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r	
	def row: Parser[Array[Int]] = number.+ ^^ {_.toArray}
	def solution: Parser[Solution] = identifier ~> number ~ row ^^ {
	  case ms ~ r => new Solution(r)
	}	
	def apply(input: String): Option[Solution] = parseAll(solution, input) match {
    	case Success(result, _) => Some(result)
    	case NoSuccess(_, _) => None
	}

}

object EvaluatedSolutionParser extends RegexParsers {

	def number: Parser[Int] = """\d+""".r ^^ { _.toInt-1 }
	def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r	
	def row: Parser[Array[Int]] = number.+ ^^ {_.toArray}
	def solution: Parser[EvaluatedSolution] = identifier ~> number ~ row ^^ {
	  case ms ~ r => new EvaluatedSolution(ms,r)
	}	
	def apply(input: String): Option[EvaluatedSolution] = parseAll(solution, input) match {
    	case Success(result, _) => Some(result)
    	case NoSuccess(_, _) => None
	}

}
