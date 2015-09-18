package it.polimi.hyperh.worksheets
import scala.util.Random
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution

object testFramework {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(221); 
  println("Welcome to the Scala worksheet");$skip(79); 
  
  val evsol = new EvaluatedSolution(9,Array(1,2,4)).asInstanceOf[Solution];System.out.println("""evsol  : it.polimi.hyperh.solution.Solution = """ + $show(evsol ));$skip(39); 
  val sol = new Solution(Array(1,2,4));System.out.println("""sol  : it.polimi.hyperh.solution.Solution = """ + $show(sol ));$skip(42); val res$0 = 

  evsol.isInstanceOf[EvaluatedSolution];System.out.println("""res0: Boolean = """ + $show(res$0));$skip(38); val res$1 = 
  sol.isInstanceOf[EvaluatedSolution];System.out.println("""res1: Boolean = """ + $show(res$1));$skip(92); 
  
  def isSubclass(instance: Solution) = {
  	instance.isInstanceOf[EvaluatedSolution]
  };System.out.println("""isSubclass: (instance: it.polimi.hyperh.solution.Solution)Boolean""");$skip(20); val res$2 = 
  isSubclass(evsol);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(18); val res$3 = 
  isSubclass(sol);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(65); val res$4 = 
  if(isSubclass(evsol))
  	evsol.asInstanceOf[EvaluatedSolution];System.out.println("""res4: Any = """ + $show(res$4))}
}
