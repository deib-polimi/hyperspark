package pfsp.util

import scala.util.parsing.combinator.RegexParsers
import pfsp.solution.PfsSolution
import pfsp.solution.PfsEvaluatedSolution

object PfsSolutionParser extends RegexParsers {

	def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
	def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r	
	def row: Parser[Array[Int]] = number.+ ^^ {_.toArray}
	def solution: Parser[PfsSolution] = identifier ~> number ~ row ^^ {
	  case ms ~ r => new PfsSolution(r)
	}	
	def apply(input: String): Option[PfsSolution] = parseAll(solution, input) match {
    	case Success(result, _) => Some(result)
    	case NoSuccess(_, _) => None
	}

}

object PfsEvaluatedSolutionParser extends RegexParsers {

	def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
	def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r	
	def row: Parser[Array[Int]] = number.+ ^^ {_.toArray}
	def solution: Parser[PfsEvaluatedSolution] = identifier ~> number ~ row ^^ {
	  case ms ~ r => new PfsEvaluatedSolution(ms,r)
	}	
	def apply(input: String): Option[PfsEvaluatedSolution] = parseAll(solution, input) match {
    	case Success(result, _) => Some(result)
    	case NoSuccess(_, _) => None
	}

}
