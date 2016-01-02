package pfsp.util

import scala.util.parsing.combinator.RegexParsers
import pfsp.problem.PfsProblem

object PfsProblemParser extends RegexParsers {

	def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
	def params: Parser[(Int,Int)] = number ~ number <~ "x" ^^ {case x ~ y => (x,y)}
	def row: Parser[Array[Int]] = number.+ <~ "x" ^^ {_.toArray}
	def matrix: Parser[Array[Array[Int]]] = row.+ ^^ {_.toArray}
	def problem: Parser[PfsProblem] = params ~ matrix ^^ {
	  case p ~ m => new PfsProblem(p._1,p._2,m)
	}
	def apply(input: String): Option[PfsProblem] = parseAll(problem, input) match {
    	case Success(result, _) => Some(result)
    	case NoSuccess(_, _) => None
	}
}

object DelphiProblemParser extends RegexParsers {
  //Source.fromFile(path).getLines().mkString(" x ") + " x ").getOrElse(throw new RuntimeException("ParserError")
  def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def params: Parser[(Int,Int)] = number ~ number <~ "x" ^^ {case x ~ y => (x,y)}
  def entry: Parser[Int] = number ~ number ^^ {case x ~ y => y}
  def row: Parser[Array[Int]] = entry.+ <~ "x" ^^ {_.toArray}
  def matrix: Parser[Array[Array[Int]]] = row.+ ^^ {_.toArray}
  def getColumn(ind: Int, m: Array[Array[Int]]): Array[Int] = m.map{_(ind)}
  def transpose(m: Array[Array[Int]]): Array[Array[Int]] = {
   var transposed: Array[Array[Int]] = Array()
   val numRows = m(0).size//5
   for(i <- 0 until numRows)
     transposed :+= getColumn(i, m)
   transposed
  }
  def problem: Parser[PfsProblem] = params ~ matrix ^^ {
    case p ~ m => new PfsProblem(p._1,p._2,transpose(m))
  }
  def apply(input: String): Option[PfsProblem] = parseAll(problem, input) match {
      case Success(result, _) => Some(result)
      case NoSuccess(_, _) => None
  }
}