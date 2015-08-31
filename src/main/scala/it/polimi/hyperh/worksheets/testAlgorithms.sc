package it.polimi.hyperh
import scala.util.Random
import it.polimi.hyperh.algorithms.NEHAlgorithm
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.algorithms.TSAlgorithm

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
  }                                               //> crossoverLOX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int]
                                                  //| )
  //crossoverLOX(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3))
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
  //crossoverPMX(List(3,9,5,4,6,2,7,1,8),List(7,4,3,8,9,2,1,5,6))

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
  //crossoverC1(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3))
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
	//crossoverNABEL(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3))
	
	def SWAP(list: List[Int]): List[Int] = {
		val firstPoint = Random.nextInt(list.size) //[0,n-1]
		var secondPoint = firstPoint
		while (secondPoint == firstPoint) { //second point must be different than first
		      secondPoint = Random.nextInt(list.size)
    }
    val result = list.toArray
    val tmp = result(firstPoint)
    result(firstPoint) = result(secondPoint)
    result(secondPoint) = tmp
    result.toList
  }                                               //> SWAP: (list: List[Int])List[Int]
  //SWAP(List(2,6,4,7,3,5,8,9,1))
  def INV(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
		val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint) //[firstPoint+1,n]
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(firstPoint).take(secondPoint - firstPoint).reverse
    val resultPart3 = list.drop(secondPoint)
    resultPart1 ::: resultPart2 ::: resultPart3
  }                                               //> INV: (list: List[Int])List[Int]
  //INV(List(2,6,4,7,3,5,8,9,1))
 def BckINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1)//[firstPoint+1,n]
		//println(firstPoint+","+secondPoint)
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(secondPoint).take(1)
    val resultPart3 = list.drop(firstPoint).filterNot(resultPart2.toSet)
		val result = resultPart1 ::: resultPart2 ::: resultPart3
		result
  }                                               //> BckINS: (list: List[Int])List[Int]
  //BckINS(List(2,6,4,7,3,5,8,9,1))
  def FwINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1) //[firstPoint+1,n]
		//println(firstPoint+","+secondPoint)
    val el1 = list.drop(firstPoint).take(1)
    val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
    val resultPart2 = list.drop(secondPoint+1)
    val result = resultPart1 ::: el1 ::: resultPart2
    result
  }                                               //> FwINS: (list: List[Int])List[Int]
  //FwINS(List(2,6,4,7,3,5,8,9,1))
  def INS(list: List[Int]): List[Int] = {
		val firstPoint = Random.nextInt(list.size) //[0,n-1]
		var secondPoint = firstPoint
		while (secondPoint == firstPoint) { //second point must be different than first
		      secondPoint = Random.nextInt(list.size)
    }
    val el1 = list.drop(firstPoint).take(1)
    if(firstPoint < secondPoint) {//FwINS
    	val el1 = list.drop(firstPoint).take(1)
	    val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
	    val resultPart2 = list.drop(secondPoint+1)
	    val result = resultPart1 ::: el1 ::: resultPart2
	    result
    }
    else {//BckINS
    	val bckInsFP = secondPoint
    	val bckInsSP = firstPoint
	    val resultPart1 = list.take(bckInsFP)
	    val resultPart2 = list.drop(bckInsSP).take(1)
	    val resultPart3 = list.drop(bckInsFP).filterNot(resultPart2.toSet)
			val result = resultPart1 ::: resultPart2 ::: resultPart3
			result
    }
	}                                         //> INS: (list: List[Int])List[Int]
  def SHIFT(list: List[Int]): List[Int] = {
  	val randomNo = Random.nextDouble()
  	if(randomNo < 0.5)
  		BckINS(list)
  	else
  		FwINS(list)
  }                                               //> SHIFT: (list: List[Int])List[Int]
  //SHIFT(List(2,6,4,7,3,5,8,9,1))
  def SWAPreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
		val firstPoint = Random.nextInt(list.size) //[0,n-1]
		var secondPoint = firstPoint
		while (secondPoint == firstPoint) { //second point must be different than first
		      secondPoint = Random.nextInt(list.size)
    }
    val pair = (firstPoint, secondPoint)
    val result = list.toArray
    val tmp = result(firstPoint)
    result(firstPoint) = result(secondPoint)
    result(secondPoint) = tmp
    (result.toList, pair)
  }                                               //> SWAPreturnMove: (list: List[Int])(List[Int], (Int, Int))
  //SWAPreturnMove(List(2,6,4,7,3,5,8,9,1))
  def INVreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
		val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint) //[firstPoint+1,n]
		val pair = (firstPoint, secondPoint)
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(firstPoint).take(secondPoint - firstPoint).reverse
    val resultPart3 = list.drop(secondPoint)
    val result = resultPart1 ::: resultPart2 ::: resultPart3
    (result, pair)
  }                                               //> INVreturnMove: (list: List[Int])(List[Int], (Int, Int))
  //INVreturnMove(List(2,6,4,7,3,5,8,9,1))
  def BckINSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
  	val firstPoint = Random.nextInt(list.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1)//[firstPoint+1,n]
		val pair = (firstPoint, secondPoint)
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(secondPoint).take(1)
    val resultPart3 = list.drop(firstPoint).filterNot(resultPart2.toSet)
		val result = resultPart1 ::: resultPart2 ::: resultPart3
		(result, pair)
  }                                               //> BckINSreturnMove: (list: List[Int])(List[Int], (Int, Int))
  //BckINSreturnMove(List(2,6,4,7,3,5,8,9,1))
  def FwINSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1) //[firstPoint+1,n]
		val pair = (firstPoint, secondPoint)
    val el1 = list.drop(firstPoint).take(1)
    val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
    val resultPart2 = list.drop(secondPoint+1)
    val result = resultPart1 ::: el1 ::: resultPart2
    (result, pair)
  }                                               //> FwINSreturnMove: (list: List[Int])(List[Int], (Int, Int))
  //FwINSreturnMove(List(2,6,4,7,3,5,8,9,1))
	def INSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
		val firstPoint = Random.nextInt(list.size) //[0,n-1]
		var secondPoint = firstPoint
		while (secondPoint == firstPoint) { //second point must be different than first
		      secondPoint = Random.nextInt(list.size)
    }
    val pair = (firstPoint, secondPoint)
    val el1 = list.drop(firstPoint).take(1)
    if(firstPoint < secondPoint) {//FwINS
    	val el1 = list.drop(firstPoint).take(1)
	    val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
	    val resultPart2 = list.drop(secondPoint+1)
	    val result = resultPart1 ::: el1 ::: resultPart2
	    (result, pair)
    }
    else {//BckINS
    	val bckInsFP = secondPoint
    	val bckInsSP = firstPoint
	    val resultPart1 = list.take(bckInsFP)
	    val resultPart2 = list.drop(bckInsSP).take(1)
	    val resultPart3 = list.drop(bckInsFP).filterNot(resultPart2.toSet)
			val result = resultPart1 ::: resultPart2 ::: resultPart3
			(result, pair)
    }
	}                                         //> INSreturnMove: (list: List[Int])(List[Int], (Int, Int))
	//INSreturnMove(List(2,6,4,7,3,5,8,9,1))
  def SHIFTreturnMove(list: List[Int]): (List[Int], (Int,Int)) = {
  	val randomNo = Random.nextDouble()
  	if(randomNo < 0.5)
  		BckINSreturnMove(list)
  	else
  		FwINSreturnMove(list)
  }                                               //> SHIFTreturnMove: (list: List[Int])(List[Int], (Int, Int))
  //SHIFTreturnMove(List(2,6,4,7,3,5,8,9,1))
  def generateNRandomNeighbourhoodMoves(numOfJobs: Int, N: Int, tabooList: List[(Int, Int)]): List[(Int, Int)] = {
    var movesList: List[(Int, Int)] = List()
    var i = 0
    while (i < N) {
      val move = NeighbourhoodSearch.randomNeighbourPair(numOfJobs) //firstPoint: [0,numOfJobs-1],secondPoint:  [0, numOfJobs-1], firstPoint!=secondPoint
      if(! tabooList.contains(move)) {
        movesList = movesList ::: List(move)
        i = i + 1
      }
    }
    movesList
  }                                               //> generateNRandomNeighbourhoodMoves: (numOfJobs: Int, N: Int, tabooList: Lis
                                                  //| t[(Int, Int)])List[(Int, Int)]
  generateNRandomNeighbourhoodMoves(20, 10, List((9,1)))
                                                  //> res0: List[(Int, Int)] = List((18,7), (0,14), (6,5), (3,19), (11,10), (7,1
                                                  //| 4), (12,5), (4,15), (0,8), (4,3))
  List().contains((1,3))                          //> res1: Boolean = false
	


 	
}