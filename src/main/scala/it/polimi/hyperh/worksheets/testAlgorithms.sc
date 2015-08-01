package it.polimi.hyperh
import scala.util.Random
import it.polimi.hyperh.algorithms.NEHAlgorithm
import it.polimi.hyperh.solution.EvaluatedSolution

object testAlgorithms {
	println("Welcome to the scala worksheet") //> Welcome to the scala worksheet
	
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
  }                                               //> crossoverLOX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])
                                                  //| 
  crossoverLOX(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3))
                                                  //> res0: (List[Int], List[Int]) = (List(4, 5, 2, 6, 7, 3, 8, 9, 1),List(2, 6, 
                                                  //| 4, 5, 1, 8, 7, 9, 3))
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
  }                                               //> crossoverPMX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int]
                                                  //| )
  crossoverPMX(List(3,9,5,4,6,2,7,1,8),List(7,4,3,8,9,2,1,5,6))
                                                  //> res1: (List[Int], List[Int]) = (List(3, 6, 1, 4, 9, 2, 1, 5, 6),List(1, 4, 
                                                  //| 3, 6, 6, 2, 7, 1, 8))

	def crossoverC1(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
    val crossoverPoint = 1 + Random.nextInt(parent1.size - 2)//[1,n-2]
    val p1Same = parent1.take(crossoverPoint)//crossoverPoint elements remains the same, fill the rest
    val p2Same = parent2.take(crossoverPoint)
    val p1Add = parent2.filterNot(p1Same.toSet)
    val p2Add = parent1.filterNot(p2Same.toSet)
    val child1 = p1Same ::: p1Add
    val child2 = p2Same ::: p2Add
    (child1, child2)
  }                                               //> crossoverC1: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])
                                                  //| 
  crossoverC1(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3))
                                                  //> res2: (List[Int], List[Int]) = (List(2, 6, 4, 7, 3, 5, 8, 1, 9),List(4, 5, 
                                                  //| 2, 1, 8, 7, 6, 3, 9))
	def crossoverNABEL(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
		val child1 = Array.ofDim[Int](parent1.size)
		val child2 = Array.ofDim[Int](parent2.size)
		for(i <- 0 until parent1.size) {
			child1(i) = parent1(parent2(i)-1)
			child2(i) = parent2(parent1(i)-1)
		}
		(child1.toList, child2.toList)
	}                                         //> crossoverNABEL: (parent1: List[Int], parent2: List[Int])(List[Int], List[In
                                                  //| t])
	crossoverNABEL(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3))
                                                  //> res3: (List[Int], List[Int]) = (List(7, 3, 6, 2, 9, 8, 5, 1, 4),List(5, 7, 
                                                  //| 1, 6, 2, 8, 9, 3, 4))
	
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
	}                                         //> mutationSWAP: (parent: List[Int])List[Int]
	mutationSWAP(List(2,6,4,7,3,5,8,9,1))     //> res4: List[Int] = List(6, 2, 4, 7, 3, 5, 8, 9, 1)
	
	def mutationINV(parent: List[Int]):List[Int] = {
    val firstPoint = Random.nextInt(parent.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent.size - firstPoint)//[firstPoint+1,n]
    val mutatedPart1 = parent.take(firstPoint)
    val mutatedPart2 = parent.drop(firstPoint).take(secondPoint-firstPoint).reverse
    val mutatedPart3 = parent.drop(secondPoint)
    mutatedPart1 ::: mutatedPart2 ::: mutatedPart3
	}                                         //> mutationINV: (parent: List[Int])List[Int]
	mutationINV(List(2,6,4,7,3,5,8,9,1))      //> res5: List[Int] = List(2, 6, 1, 9, 8, 5, 3, 7, 4)
	
	def mutationINS(parent: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(parent.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(parent.size - firstPoint - 1)//[firstPoint+1,n]
		println(firstPoint+","+secondPoint)
    val mutatedPart1 = parent.take(firstPoint)
    val mutatedPart2 = parent.drop(secondPoint).take(1)
    val mutatedPart3 = parent.drop(firstPoint).filterNot(mutatedPart2.toSet)
		val mutated = mutatedPart1 ::: mutatedPart2 ::: mutatedPart3
		mutated
	}                                         //> mutationINS: (parent: List[Int])List[Int]
	mutationINS(List(2,6,4,7,3,5,8,9,1))      //> 6,8
                                                  //| res6: List[Int] = List(2, 6, 4, 7, 3, 5, 1, 8, 9)
}