package it.polimi.hyperh.worksheets
import scala.util.Random
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Logger

object testFramework {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
 	def checkDuplicates(list: List[Int]): Boolean = {
 		list.distinct.size != list.size
 	}                                         //> checkDuplicates: (list: List[Int])Boolean
 	val logger = Logger()                     //> logger  : util.Logger = util.Logger@3abbfa04
 	logger.setFormat(List("instance","n","m","algorithmName","parallelism","totalTime(s)","makespan","best","rpd","mode"))
 	logger.printFormat()                      //> instance       	n              	m              	algorithmName  	parallel
                                                  //| ism    	totalTime(s)   	makespan       	best           	rpd            	
                                                  //| mode           	
  logger.printValues(List("inst_ta001",20,5,"IGAlgorithm",4, "3.0", 1356, 1278, 2.40, "parallel"))
                                                  //> inst_ta001     	20             	5              	IGAlgorithm    	4       
                                                  //|        	3.0            	1356           	1278           	2.4            	
                                                  //| parallel       	
}