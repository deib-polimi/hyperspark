package it.polimi.hyperh
import scala.util.Random

object testAlgorithms {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(117); 
	println("Welcome to the scala worksheet");$skip(746); 
	def crossoverLOX(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
    val size = parent1.size
    val firstPoint = Random.nextInt(size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(size - firstPoint)//[firstPoint+1,n]
    val p1Remove = parent2.drop(firstPoint).take(secondPoint-firstPoint)
    val p2Remove = parent1.drop(firstPoint).take(secondPoint-firstPoint)
    val p1Filtered = parent1.filterNot(p1Remove.toSet)
    val p2Filtered = parent2.filterNot(p2Remove.toSet)
    val p1Reconstructed = p1Filtered.take(firstPoint):::p1Remove:::p1Filtered.drop(firstPoint)
    val p2Reconstructed = p2Filtered.take(firstPoint):::p2Remove:::p2Filtered.drop(firstPoint)
    (p1Reconstructed, p2Reconstructed)
  };System.out.println("""crossoverLOX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(64); val res$0 = 
  crossoverLOX(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3));System.out.println("""res0: (List[Int], List[Int]) = """ + $show(res$0));$skip(1362); 
  //https://books.google.it/books?id=j5_kKgpjMBQC&pg=PA65&lpg=PA65&dq=linear+order+crossover+and+partially+mapped+crossover+same&source=bl&ots=hlkfaRCoe0&sig=_lrXIS_d-Bskx-fskTtR5sckOH0&hl=en&sa=X&ved=0CCcQ6AEwAWoVChMImKGrtYf3xgIVQ8AUCh0IUwDw#v=onepage&q=linear%20order%20crossover%20and%20partially%20mapped%20crossover%20same&f=false
  def crossoverPMX(parent1:List[Int], parent2: List[Int])/*:(List[Int],List[Int])*/ = {
    val size = parent1.size
    val firstPoint = 2//Random.nextInt(size - 1)//[0,n-2]
    val secondPoint = 6//firstPoint + 1 + Random.nextInt(size - firstPoint)//[firstPoint+1,n]
    val mapPart1 = parent1.drop(firstPoint).take(secondPoint-firstPoint)
    val mapPart2 = parent2.drop(firstPoint).take(secondPoint-firstPoint)
    val mappings = ((mapPart1 zip mapPart2) ::: (mapPart2 zip mapPart1)).toSet
    
    def applyMapping(parent:List[Int], mappings: Set[(Int,Int)],result:List[Int]):List[Int] = parent match {
	    case List()  => result
	    case x :: xs => mappings.filter(y => y._1 == x).toList match {
	    	case List() => applyMapping(xs, mappings, result ::: List(x))
	    	case List(m) => applyMapping(xs, mappings.filter(y => y._1 != m._1 && y._2 != m._2), result ::: List(m._2))
	    }
		}
		val child1 = applyMapping(parent1,mappings, List())
		val child2 = applyMapping(parent2,mappings, List())
		(child1,child2)
  };System.out.println("""crossoverPMX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(64); val res$1 = 
  crossoverPMX(List(3,9,5,4,6,2,7,1,8),List(7,4,3,8,9,2,1,5,6));System.out.println("""res1: (List[Int], List[Int]) = """ + $show(res$1))}

  /*def applyMapping(parent:List[Int], mappings: Set[(Int,Int)],result:List[Int]):List[Int] = parent match {
    case List()  => result
    case x :: xs => mappings.filter(y => y._1 == x).toList match {
    	case List() => applyMapping(xs, mappings, result ::: List(x))
    	case List(m) => applyMapping(xs, mappings.filter(y => y._1 != m._1 && y._2 != m._2), result ::: List(m._2))
    }
	}  */
	//val mappings =  List((5,3), (4,8), (6,9), (2,2), (3,5), (8,4), (9,6), (2,2))
	//applyMapping(List(3,9,5,4,6,2,7,1,8),mappings.toSet, List())
}
