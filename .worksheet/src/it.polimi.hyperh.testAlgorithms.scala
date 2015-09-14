package it.polimi.hyperh
import scala.util.Random
import it.polimi.hyperh.algorithms.NEHAlgorithm
import it.polimi.hyperh.solution.EvaluatedSolution
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.algorithms.TSAlgorithm
import it.polimi.hyperh.problem.Problem
import util.ConsolePrinter

object testAlgorithms {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(381); 
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
  };System.out.println("""crossoverLOX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(1756); 
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
  };System.out.println("""crossoverPMX: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(558); 
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
  };System.out.println("""crossoverC1: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(391); 
  //crossoverC1(List(2,6,4,7,3,5,8,9,1),List(4,5,2,1,8,7,6,9,3))
	def crossoverNABEL(parent1:List[Int], parent2: List[Int]):(List[Int],List[Int]) = {
		val child1 = Array.ofDim[Int](parent1.size)
		val child2 = Array.ofDim[Int](parent2.size)
		for(i <- 0 until parent1.size) {
			child1(i) = parent1(parent2(i)-1)
			child2(i) = parent2(parent1(i)-1)
		}
		(child1.toList, child2.toList)
	};System.out.println("""crossoverNABEL: (parent1: List[Int], parent2: List[Int])(List[Int], List[Int])""");$skip(493); 
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
  };System.out.println("""SWAP: (list: List[Int])List[Int]""");$skip(456); 
  //SWAP(List(2,6,4,7,3,5,8,9,1))
  def INV(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
		val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint) //[firstPoint+1,n]
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(firstPoint).take(secondPoint - firstPoint).reverse
    val resultPart3 = list.drop(secondPoint)
    resultPart1 ::: resultPart2 ::: resultPart3
  };System.out.println("""INV: (list: List[Int])List[Int]""");$skip(519); 
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
  };System.out.println("""BckINS: (list: List[Int])List[Int]""");$skip(509); 
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
  };System.out.println("""FwINS: (list: List[Int])List[Int]""");$skip(946); 
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
	};System.out.println("""INS: (list: List[Int])List[Int]""");$skip(149); 
  def SHIFT(list: List[Int]): List[Int] = {
  	val randomNo = Random.nextDouble()
  	if(randomNo < 0.5)
  		BckINS(list)
  	else
  		FwINS(list)
  };System.out.println("""SHIFT: (list: List[Int])List[Int]""");$skip(27); 
  
  val numOfMachines = 6;System.out.println("""numOfMachines  : Int = """ + $show(numOfMachines ));$skip(20); 
  val numOfJobs = 8;System.out.println("""numOfJobs  : Int = """ + $show(numOfJobs ));$skip(363); 
  
 	def produceArcs(numOfJobs: Int, numOfMachines: Int): List[((Int, Int), (Int, Int))] = {
 		var arcs: List[((Int, Int), (Int, Int))] = List()
 		for(x <- 1 to numOfMachines; y <- 1 to numOfJobs) {
 			if((y+1) <= numOfJobs)
 				arcs = arcs ::: List(((x,y),(x,y+1)))
 			if((x+1) <= numOfMachines)
 				arcs = arcs ::: List(((x,y),(x+1,y)))
 		}
 		arcs
 	};System.out.println("""produceArcs: (numOfJobs: Int, numOfMachines: Int)List[((Int, Int), (Int, Int))]""");$skip(153); 
 	def getMovesFrom(node: (Int, Int), arcs: List[((Int, Int), (Int, Int))]): List[((Int, Int), (Int, Int))] = {
 		arcs.filter(arc => arc._1 == node)
 	};System.out.println("""getMovesFrom: (node: (Int, Int), arcs: List[((Int, Int), (Int, Int))])List[((Int, Int), (Int, Int))]""");$skip(240); 
 	//getMovesFrom((3,2), arcs)
 	//getMovesFrom((3,3), arcs)
 	def getNodeWeight(node: (Int, Int), jobTimesMatrix: Array[Array[Int]], jobsPermutation: Array[Int]): Int = {
 		jobTimesMatrix(node._1 - 1)(jobsPermutation(node._2 - 1) - 1)
 	};System.out.println("""getNodeWeight: (node: (Int, Int), jobTimesMatrix: Array[Array[Int]], jobsPermutation: Array[Int])Int""");$skip(869); 
 	
  
  
  def getBlockLimits(path: List[(Int, Int)]): List[(Int, (Int,Int))] = {
  	var blocks: List[(Int, (Int,Int))] = List()
  	var i = 1
  	var same = false
  	var machine = path(0)._1
  	var leftPos = path(0)._2
  	var rightPos = path(0)._2
  	while(i < path.size) {
  		while((path(i-1)._1 != path(i)._1) && (i+1 < path.size)) {
  		
				i = i + 1
			}
  		machine = path(i)._1
  		leftPos = path(i-1)._2
  		rightPos = path(i)._2
			var notExceeded = true
			while(notExceeded && (path(i-1)._1 == path(i)._1)) {
				machine = path(i)._1
				rightPos = path(i)._2
				i = i + 1
  			if(i >= path.size)
  				notExceeded = false
			}
			if(rightPos - leftPos > 0) {
				if(notExceeded)
	  				rightPos = path(i)._2
	  			else rightPos = path(path.size-1)._2
  				blocks = blocks ::: List( (machine,(leftPos,rightPos)) )
  			}
  		i = i + 1
  	}
  	blocks
  };System.out.println("""getBlockLimits: (path: List[(Int, Int)])List[(Int, (Int, Int))]""");$skip(114); 
	val blocks = getBlockLimits(List((1,1),(2,1),(2,2),(3,2),(3,3),(3,4),(3,5),(4,5),(5,5),(5,6),(6,6),(6,7),(6,8)));System.out.println("""blocks  : List[(Int, (Int, Int))] = """ + $show(blocks ));$skip(106); 
  def getMachines(blocks  : List[(Int, (Int, Int))]): Array[Int] = {
  	blocks.map(t => t._1).toArray
  };System.out.println("""getMachines: (blocks: List[(Int, (Int, Int))])Array[Int]""");$skip(32); 
	val mArr = getMachines(blocks);System.out.println("""mArr  : Array[Int] = """ + $show(mArr ));$skip(167); 
	def getU(blocks  : List[(Int, (Int, Int))]): Array[Int] = {
		blocks.filter(p => p._2._1 < p._2._2).map(p => p._2).flatMap(t => List(t._1, t._2)).distinct.toArray
	};System.out.println("""getU: (blocks: List[(Int, (Int, Int))])Array[Int]""");$skip(25); 
	val uArr = getU(blocks);System.out.println("""uArr  : Array[Int] = """ + $show(uArr ));$skip(259); 
	
	//returns the greatest index on the right inside the block containing jPos
  def lr(jPos: Int, u: Array[Int]): Int = {
  	var index = -999999
  	for(i <- 0 until u.size - 1) {
  		if(u(i) <= jPos && jPos < u(i + 1))
  			index = u(i + 1)
  	}
  	index
  };System.out.println("""lr: (jPos: Int, u: Array[Int])Int""");$skip(235); 
  def ll(jPos: Int, u: Array[Int]): Int = {
  	var index = -999999
  	for(i <- 0 until u.size - 1) {
  		if(u(i) < jPos && jPos <= u(i + 1))
  			index = u(i)
  	}
  	if(jPos < 2 || jPos > u(u.size-1))
  		index = -999999
  	index
  };System.out.println("""ll: (jPos: Int, u: Array[Int])Int""");$skip(16); val res$0 = 
  
  ll(5,uArr);System.out.println("""res0: Int = """ + $show(res$0));$skip(14); val res$1 = 
  lr(5, uArr);System.out.println("""res1: Int = """ + $show(res$1));$skip(266); 
	
  def calculateDelta(u: Array[Int], epsilon: Double): Array[Int] = {
  	val k = u.size - 1
  	val delta: Array[Int] = Array.ofDim[Int](k + 2)
  	delta(0) = 0
  	for(l <- 1 to k)
  			delta(l) = ((u(l) - u(l - 1)) * epsilon).toInt
  	delta(k + 1) = 0
    delta
  };System.out.println("""calculateDelta: (u: Array[Int], epsilon: Double)Array[Int]""");$skip(538); 

 	def calculateZRJ(jPos: Int, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
 		if(jPos < 1 || jPos > uArr(uArr.size-1) - 1)
 			List()
 		if(mArr(mArr.size - 1) == numOfMachines && (jPos >= (uArr(uArr.size - 2) + 1)))
 			List()
 		else {
	  	val delta = calculateDelta(uArr, epsilon)
	  	val lright = lr(jPos, uArr)
	  	val deltaNext = delta(uArr.indexOf(lright)+1)
	  	var sum = lright + deltaNext
	  	if (sum > numOfJobs)
	  		sum = numOfJobs
	  	(for(t <- lright to sum) yield (jPos, t)).toList
	  }
  };System.out.println("""calculateZRJ: (jPos: Int, uArr: Array[Int], mArr: Array[Int], epsilon: Double)List[(Int, Int)]""");$skip(830); 
  /*for(i <- 1 to 8) {
  	println(i +": "+calculateZRJ(i, uArr, mArr, 0.0))
  }
  for(i <- 1 to 8) {
  	println(i +": "+calculateZRJ(i, uArr, mArr, 1.0))
  }*/
  def calculateZLJ(jPos: Int, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
  	def w(x: Int) = if(x > 1) 0 else 1
 		if(jPos < 2 || jPos > uArr(uArr.size-1)) {
 			List()
 		}
 		else {
	 		if(mArr(0) == 1 && (jPos <= (uArr(1) - 1))) {
	 			List()
	 		} else {
		  	val delta = calculateDelta(uArr, epsilon)
		  	val lleft = ll(jPos, uArr)
		  	val deltaL = delta(uArr.indexOf(lleft))
		  	val lNext = uArr(uArr.indexOf(lleft) + 1)
		  	var diff1 = lleft - deltaL
		  	if (diff1 < 1)
		  		diff1 = 1
		  	val diff2 = lleft - w(lNext - lleft)
		  	(for(t <- diff1 to diff2) yield (jPos, t)).toList.filterNot(p => p._1 == p._2)
		  }
	  }
  };System.out.println("""calculateZLJ: (jPos: Int, uArr: Array[Int], mArr: Array[Int], epsilon: Double)List[(Int, Int)]""");$skip(420); 
	/*for(i <- 1 to 8) {
  	println(i +": "+calculateZLJ(i, uArr, mArr, 0.0))
  }*/
  /*for(i <- 1 to 8) {
  	println(i +": "+calculateZLJ(i, uArr, mArr, 1.0))
  }*/
	def calculateZR(uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
		var movesRight: List[(Int, Int)] = List()
		for(j <- 1 to numOfJobs - 1) {
			movesRight = movesRight ::: calculateZRJ(j, uArr, mArr, epsilon)
		}
		movesRight
	};System.out.println("""calculateZR: (uArr: Array[Int], mArr: Array[Int], epsilon: Double)List[(Int, Int)]""");$skip(252); 
  
	def calculateZL(uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
		var movesLeft: List[(Int, Int)] = List()
		for(j <- 2 to numOfJobs) {
			movesLeft = movesLeft ::: calculateZLJ(j, uArr, mArr, epsilon)
		}
		movesLeft
	};System.out.println("""calculateZL: (uArr: Array[Int], mArr: Array[Int], epsilon: Double)List[(Int, Int)]""");$skip(166); 
	def calculateZ(uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
		calculateZR(uArr, mArr, epsilon) ::: calculateZL(uArr, mArr, epsilon)
	};System.out.println("""calculateZ: (uArr: Array[Int], mArr: Array[Int], epsilon: Double)List[(Int, Int)]""");$skip(66); val res$2 = 
  //calculateZ(uArr, mArr, 0.0).size
	calculateZ(uArr, mArr, 1.0);System.out.println("""res2: List[(Int, Int)] = """ + $show(res$2));$skip(356); 
	def getNodesFrom(node: (Int, Int), numOfMachines: Int, numOfJobs: Int): List[((Int, Int), (Int, Int))] = {
    var list: List[((Int, Int), (Int, Int))] = List()
    val x = node._1
    val y = node._2
	  if((y+1) <= numOfJobs)
	    list = list ::: List(((x,y),(x,y+1)))
	  if((x+1) <= numOfMachines)
	    list = list ::: List(((x,y),(x+1,y)))
	  list
  };System.out.println("""getNodesFrom: (node: (Int, Int), numOfMachines: Int, numOfJobs: Int)List[((Int, Int), (Int, Int))]""");$skip(83); 
  
	val problem = new Problem(3,3,Array(Array(1,2,1), Array(1,1,2),Array(2,2,1)));System.out.println("""problem  : it.polimi.hyperh.problem.Problem = """ + $show(problem ));$skip(486); 
  
  def critMatrix(p: Problem, jobsPermutation: Array[Int]): Array[Array[Int]] = {
    val R = Array.ofDim[Int](p.numOfMachines, p.numOfJobs)
    R(0)(0) = p.jobTimesMatrix(0)(0)
    def rgh(g: Int, h: Int): Int = {
      if(g == -1 || h == -1)
        0
      else
        R(g)(h)//0..m-1,0..n-1
    }
    for(h <- 0 until p.numOfJobs; g <- 0 until p.numOfMachines) {
      R(g)(h) = scala.math.max(rgh(g,h-1), rgh(g-1,h)) + p.jobTimesMatrix(g)(jobsPermutation(h)-1)
    }
    R
  };System.out.println("""critMatrix: (p: it.polimi.hyperh.problem.Problem, jobsPermutation: Array[Int])Array[Array[Int]]""");$skip(748); 
   def criticalPath(p: Problem, jobsPermutation: Array[Int]) = {
    val R = critMatrix(p, jobsPermutation)
    def rgh(g: Int, h: Int): Int = {
      if (g == 0 || h == 0)
        0
      else
        R(g - 1)(h - 1) //1..m,1..n
    }
    def mymax(move1: (Int, Int), move2: (Int, Int)): (Int, Int) = {
    	if (rgh(move1._1, move1._2) > rgh(move2._1, move2._2))
    		move1
    	else move2
    }
    var move = (p.numOfMachines, p.numOfJobs) //start from bottom right corner
    var path: List[(Int, Int)] = List(move)
    while (move._1 != 1 || move._2 != 1) {
      val better = mymax((move._1 - 1, move._2), (move._1, move._2 - 1))
      path = List(better) ::: path
      move = better
    }
    (path, rgh(p.numOfMachines, p.numOfJobs))
  };System.out.println("""criticalPath: (p: it.polimi.hyperh.problem.Problem, jobsPermutation: Array[Int])(List[(Int, Int)], Int)""")}
  /*def qgh(g: Int, h: Int): Int = {
  	
  }*/

  

              
 
}
