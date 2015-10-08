package it.polimi.hyperh.worksheets
import scala.util.Random
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Logger

object testFramework {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(240); 
  println("Welcome to the Scala worksheet");$skip(91); 
 	def checkDuplicates(list: List[Int]): Boolean = {
 		list.distinct.size != list.size
 	};System.out.println("""checkDuplicates: (list: List[Int])Boolean""");$skip(24); 
 	val logger = Logger();System.out.println("""logger  : util.Logger = """ + $show(logger ));$skip(121); 
 	logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"));$skip(23); 
 	logger.printFormat();$skip(99); 
  logger.printValues(List("inst_ta001",20,5,"IGAlgorithm",4, "3.0", 1356, 1278, 2.40, "parallel"))}
}
