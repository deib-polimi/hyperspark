package util

import scala.util.parsing.combinator.RegexParsers
import it.polimi.hyperh.problem.Problem

object ProblemParser extends RegexParsers {

	def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
	def params: Parser[(Int,Int)] = number ~ number <~ "x" ^^ {case x ~ y => (y,x)}
	def row: Parser[Array[Int]] = number.+ <~ "x" ^^ {_.toArray}
	def matrix: Parser[Array[Array[Int]]] = row.+ ^^ {_.toArray}
	def problem: Parser[Problem] = params ~ matrix ^^ {
	  case p ~ m => new Problem(p._1,p._2,m)
	}	
	def apply(input: String): Option[Problem] = parseAll(problem, input) match {
    	case Success(result, _) => Some(result)
    	case NoSuccess(_, _) => None
	}
}

