package pfsp.neighbourhood

import scala.util.Random

/**
 * @author Nemanja
 */
class NeighbourhoodOperator(random: Random) {
  def this() = {
    this(new Random(1))
  }
  //OPERATIONS
  def SWAPdefineMove(list: List[Int], firstPoint: Int, secondPoint: Int): List[Int] = {
    val result = list.toArray
    val tmp = result(firstPoint)
    result(firstPoint) = result(secondPoint)
    result(secondPoint) = tmp
    result.toList
  }
  def AdjSWAPdefineMove(list: List[Int], firstPoint: Int): List[Int] = {
    SWAPdefineMove(list, firstPoint, firstPoint + 1)
  }
  def INVdefineMove(list: List[Int], firstPoint: Int, secondPoint: Int): List[Int] = {
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(firstPoint).take(secondPoint - firstPoint).reverse
    val resultPart3 = list.drop(secondPoint)
    resultPart1 ::: resultPart2 ::: resultPart3
  }
  def BckINSdefineMove(list: List[Int], firstPoint: Int, secondPoint: Int): List[Int] = {
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(secondPoint).take(1)
    val resultPart3 = list.drop(firstPoint).filterNot(resultPart2.toSet)
    val result = resultPart1 ::: resultPart2 ::: resultPart3
    result
  }
  def FwINSdefineMove(list: List[Int], firstPoint: Int, secondPoint: Int): List[Int] = {
    val el1 = list.drop(firstPoint).take(1)
    val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
    val resultPart2 = list.drop(secondPoint+1)
    val result = resultPart1 ::: el1 ::: resultPart2
    result
  }
  def INSdefineMove(list: List[Int], firstPoint: Int, secondPoint: Int): List[Int] = {
    val el1 = list.drop(firstPoint).take(1)
    if(firstPoint < secondPoint) {//FwINS
      val el1 = list.drop(firstPoint).take(1)
      val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
      val resultPart2 = list.drop(secondPoint+1)
      val result = resultPart1 ::: el1 ::: resultPart2
      result
    }
    else {//BckINS, firstPoint > secondPoint
      val bckInsFP = secondPoint
      val bckInsSP = firstPoint
      val resultPart1 = list.take(bckInsFP)
      val resultPart2 = list.drop(bckInsSP).take(1)
      val resultPart3 = list.drop(bckInsFP).filterNot(resultPart2.toSet)
      val result = resultPart1 ::: resultPart2 ::: resultPart3
      result
    }
  }
  //RANDOM MOVES GENERATORS
  def randomZeroToNminusOne(n: Int): Int = {
    random.nextInt(n)
  }
  def randomZeroToNminusTwo(n: Int): Int = {
    random.nextInt(n - 1)
  }
  def randomNeighbourPair(n: Int): (Int, Int) = {
    val firstPoint = randomZeroToNminusOne(n) //[0,n-1]
    var secondPoint = firstPoint
    while (secondPoint == firstPoint) { //second point must be different than first
      secondPoint = randomZeroToNminusOne(n) //[0,n-1], secondPoint != firstPoint
    }
    (firstPoint, secondPoint)
  }
  def randomSuccessivePoint(firstPoint:Int, n: Int): Int = {
    firstPoint + 1 + random.nextInt(n - firstPoint - 1) //[firstPoint+1,n]
  }
  def randomSuccessivePair(n: Int): (Int, Int) = {
    val firstPoint = randomZeroToNminusTwo(n) //[0,n-2]
    val secondPoint = randomSuccessivePoint(firstPoint, n) //[firstPoint+1,n]
    (firstPoint, secondPoint)
  }
  def generateAllNeighbourhoodMoves(numOfJobs: Int): List[(Int, Int)] = {
    //tuples of distinct values, (1,1) is not allowed
    (for (x <- 0 until numOfJobs; y <- 0 until numOfJobs) yield (x, y)).toList.filter(p => p._1 != p._2)
  }
  
  def generateNRandomNeighbourhoodMoves(numOfJobs: Int, N: Int): List[(Int, Int)] = {
    var movesList: List[(Int, Int)] = List()
    var i = 0
    while (i < N) {
      val move = randomNeighbourPair(numOfJobs) //firstPoint: [0,numOfJobs-1],secondPoint:  [0, numOfJobs-1], firstPoint!=secondPoint
      movesList = movesList ::: List(move)
      i = i + 1  
    }
    movesList
  }
  //FUNCTIONS THAT RANDOMLY INITIALIZE THE MOVES (POSITIONS)
  def SWAP(list: List[Int]): List[Int] = {
    val pair = randomNeighbourPair(list.size) //firstPoint: [0,n-1],secondPoint:  [0, n-1], firstPoint!=secondPoint
    SWAPdefineMove(list, pair._1, pair._2)
  }
  def AdjSWAP(list: List[Int]): List[Int] = {
    val firstPoint = randomZeroToNminusTwo(list.size) //[0,n-2]
    AdjSWAPdefineMove(list, firstPoint)
  }
  def INV(list: List[Int]): List[Int] = {
    val pair = randomSuccessivePair(list.size) //firstPoint: [0,n-2],secondPoint:  [firstPoint+1,n]
    INVdefineMove(list, pair._1, pair._2)
  }
  def BckINS(list: List[Int]): List[Int] = {
    val pair = randomSuccessivePair(list.size) //firstPoint: [0,n-2],secondPoint:  [firstPoint+1,n]
    BckINSdefineMove(list, pair._1, pair._2)
  }
  def FwINS(list: List[Int]): List[Int] = {
    val pair = randomSuccessivePair(list.size) //firstPoint: [0,n-2],secondPoint:  [firstPoint+1,n]
    FwINSdefineMove(list, pair._1, pair._2)
  }
  
  def SHIFT(list: List[Int]): List[Int] = {
    val randomNo = random.nextDouble()
    if(randomNo < 0.5)
      BckINS(list)
    else
      FwINS(list)
  }
  def INS(list: List[Int]): List[Int] = {
    val pair = randomNeighbourPair(list.size) //firstPoint: [0,n-1],secondPoint:  [0, n-1], firstPoint!=secondPoint
    INSdefineMove(list, pair._1, pair._2)
  }
  //FUNCTIONS THAT RANDOMLY INITIALIZE THE MOVES (POSITIONS), AND 
  //IN ADDITION RETURN WHICH MOVE THEY USED
  def SWAPreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val pair = randomNeighbourPair(list.size) //firstPoint: [0,n-1],secondPoint:  [0, n-1], firstPoint!=secondPoint
    val result = SWAPdefineMove(list, pair._1, pair._2)
    (result.toList, pair)
  }
  def AdjSWAPreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = random.nextInt(list.size - 1) //[0,n-2]
    (AdjSWAPdefineMove(list, firstPoint), (firstPoint, firstPoint + 1))
  }
  def INVreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val pair = randomSuccessivePair(list.size) //firstPoint: [0,n-2],secondPoint:  [firstPoint+1,n]
    val result = INVdefineMove(list, pair._1, pair._2)
    (result, pair)
  }
  def BckINSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val pair = randomSuccessivePair(list.size) //firstPoint: [0,n-2],secondPoint:  [firstPoint+1,n]
    val result =  BckINSdefineMove(list, pair._1, pair._2)
    (result, pair)
  }
  def FwINSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val pair = randomSuccessivePair(list.size) //firstPoint: [0,n-2],secondPoint:  [firstPoint+1,n]
    val result = FwINSdefineMove(list, pair._1, pair._2)
    (result, pair)
  }
  def SHIFTreturnMove(list: List[Int]): (List[Int], (Int,Int)) = {
    val randomNo = random.nextDouble()  //[0,1]
    if(randomNo < 0.5)
      BckINSreturnMove(list)
    else
      FwINSreturnMove(list)
  }
  def INSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val pair = randomNeighbourPair(list.size) //firstPoint: [0,n-1],secondPoint:  [0, n-1], firstPoint!=secondPoint
    val el1 = list.drop(pair._1).take(1)
    if(pair._1 < pair._2) {//FwINS
      val result = FwINSdefineMove(list, pair._1, pair._2)
      (result, pair)
    }
    else {//BckINS, firstPoint > secondPoint
      //in BckINS call we need to reverse points, e.g. (5,1) becomes (1,5)
      val result = BckINSdefineMove(list, pair._2, pair._1)
      (result, pair)
    }
  }
}
object NeighbourhoodOperator {
  def apply(random: Random) = {
    new NeighbourhoodOperator(random)
  }
  def apply() = {
    new NeighbourhoodOperator()  
  }
}