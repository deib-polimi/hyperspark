package it.polimi.hyperh
import scala.util.Random
import it.polimi.hyperh.algorithms.NEHAlgorithm
import it.polimi.hyperh.solution.EvaluatedSolution

object testAlgorithms {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(216); 
	println("Welcome to the scala worksheet");$skip(749); 
	
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
  crossoverLOX(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3));System.out.println("""res0: (List[Int], List[Int]) = """ + $show(res$0));$skip(1690); 
  //https://books.google.it/books?id=j5_kKgpjMBQC&pg=PA65&lpg=PA65&dq=linear+order+crossover+and+partially+mapped+crossover+same&source=bl&ots=hlkfaRCoe0&sig=_lrXIS_d-Bskx-fskTtR5sckOH0&hl=en&sa=X&ved=0CCcQ6AEwAWoVChMImKGrtYf3xgIVQ8AUCh0IUwDw#v=onepage&q=linear%20order%20crossover%20and%20partially%20mapped%20crossover%20same&f=false
  def crossoverPMX(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
    val firstPoint = Random.nextInt(parent1.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent1.size - firstPoint)//[firstPoint+1,n]
    val child1Part1 = parent1.take(firstPoint)
    val child1Part2 = parent1.drop(firstPoint).take(secondPoint-firstPoint)
    val child1Part3 = parent1.drop(secondPoint)
    val child2Part1 = parent2.take(firstPoint)
    val child2Part2 = parent2.drop(firstPoint).take(secondPoint-firstPoint)
    val child2Part3 = parent2.drop(secondPoint)
    val mappings1 = child1Part2 zip child2Part2
    val mappings2 = child2Part2 zip child1Part2
    def applyMapping(list: List[Int], mappings: List[(Int, Int)], result: List[Int]):List[Int] = list match {
    	case List()  => result
      case head :: tail => mappings.filter(y => y._1 == head).toList match {
        case List() => applyMapping(tail, mappings, result ::: List(head))
        case m :: ms => applyMapping(tail, mappings, result ::: List(m._2))
      }
    }
    val child1 = applyMapping(child1Part1, mappings2, List()) ::: child2Part2 ::: applyMapping(child1Part3, mappings2, List())
    val child2 = applyMapping(child2Part1, mappings1, List()) ::: child1Part2 ::: applyMapping(child2Part3, mappings1, List())
		(child1.toList,child2.toList)
  };System.out.println("""crossoverPMX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(64); val res$1 = 
  crossoverPMX(List(3,9,5,4,6,2,7,1,8),List(7,4,3,8,9,2,1,5,6));System.out.println("""res1: (List[Int], List[Int]) = """ + $show(res$1));$skip(492); 

	def crossoverC1(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
    val crossoverPoint = 1 + Random.nextInt(parent1.size - 2)//[1,n-2]
    val p1Same = parent1.take(crossoverPoint)//crossoverPoint elements remains the same, fill the rest
    val p2Same = parent2.take(crossoverPoint)
    val p1Add = parent2.filterNot(p1Same.toSet)
    val p2Add = parent1.filterNot(p2Same.toSet)
    val child1 = p1Same ::: p1Add
    val child2 = p2Same ::: p2Add
    (child1, child2)
  };System.out.println("""crossoverC1: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(63); val res$2 = 
  crossoverC1(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3));System.out.println("""res2: (List[Int], List[Int]) = """ + $show(res$2));$skip(326); 
	def crossoverNABEL(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
		val child1 = Array.ofDim[Int](parent1.size)
		val child2 = Array.ofDim[Int](parent2.size)
		for(i <- 0 until parent1.size) {
			child1(i) = parent1(parent2(i)-1)
			child2(i) = parent2(parent1(i)-1)
		}
		(child1.toList, child2.toList)
	};System.out.println("""crossoverNABEL: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(65); val res$3 = 
	crossoverNABEL(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3));System.out.println("""res3: (List[Int], List[Int]) = """ + $show(res$3));$skip(427); 
	
	def mutationSWAP(parent:List[Int]): List[Int] = {
		val firstPoint = Random.nextInt(parent.size)//[0,n-1]
		var secondPoint = firstPoint
		while( secondPoint == firstPoint) {	//second point must be different than first
			secondPoint= Random.nextInt(parent.size)
		}
		val mutated = parent.toArray
		val tmp = mutated(firstPoint)
		mutated(firstPoint) = mutated(secondPoint)
		mutated(secondPoint) = tmp
		mutated.toList
	};System.out.println("""mutationSWAP: (parent: List[Int])List[Int]""");$skip(39); val res$4 = 
	mutationSWAP(List(2,6,4,7,3,5,8,9,1));System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(445); 
	
	def mutationINV(parent: List[Int]):List[Int] = {
    val firstPoint = Random.nextInt(parent.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent.size - firstPoint)//[firstPoint+1,n]
    val mutatedPart1 = parent.take(firstPoint)
    val mutatedPart2 = parent.drop(firstPoint).take(secondPoint-firstPoint).reverse
    val mutatedPart3 = parent.drop(secondPoint)
    mutatedPart1 ::: mutatedPart2 ::: mutatedPart3
	};System.out.println("""mutationINV: (parent: List[Int])List[Int]""");$skip(38); val res$5 = 
	mutationINV(List(2,6,4,7,3,5,8,9,1));System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(511); 
	
	def mutationINS(parent: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(parent.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent.size - firstPoint - 1)//[firstPoint+1,n]
		println(firstPoint+","+secondPoint)
    val mutatedPart1 = parent.take(firstPoint)
    val mutatedPart2 = parent.drop(secondPoint).take(1)
    val mutatedPart3 = parent.drop(firstPoint).filterNot(mutatedPart2.toSet)
		val mutated = mutatedPart1 ::: mutatedPart2 ::: mutatedPart3
		mutated
	};System.out.println("""mutationINS: (parent: List[Int])List[Int]""");$skip(38); val res$6 = 
	mutationINS(List(2,6,4,7,3,5,8,9,1));System.out.println("""res6: List[Int] = """ + $show(res$6));$skip(484); 
 def BckINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1)//[firstPoint+1,n]
		println(firstPoint+","+secondPoint)
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(secondPoint).take(1)
    val resultPart3 = list.drop(firstPoint).filterNot(resultPart2.toSet)
		val result = resultPart1 ::: resultPart2 ::: resultPart3
		result
  };System.out.println("""BckINS: (list: List[Int])List[Int]""");$skip(34); val res$7 = 
  BckINS(List(2,6,4,7,3,5,8,9,1));System.out.println("""res7: List[Int] = """ + $show(res$7));$skip(471); 
  def FwINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1) //[firstPoint+1,n]
		println(firstPoint+","+secondPoint)
    val el1 = list.drop(firstPoint).take(1)
    val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
    val resultPart2 = list.drop(secondPoint+1)
    val result = resultPart1 ::: el1 ::: resultPart2
    result
  };System.out.println("""FwINS: (list: List[Int])List[Int]""");$skip(33); val res$8 = 
  FwINS(List(2,6,4,7,3,5,8,9,1));System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(152); 
  
  def SHIFT(list: List[Int]): List[Int] = {
  	val randomNo = Random.nextDouble()
  	if(randomNo < 0.5)
  		BckINS(list)
  	else
  		FwINS(list)
  };System.out.println("""SHIFT: (list: List[Int])List[Int]""")}
}
