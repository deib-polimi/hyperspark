package it.polimi.hyperh.search

import scala.util.Random

/**
 * @author Nemanja
 */
object NeighbourhoodSearch {
  def SWAPdefineMove(list: List[Int], firstPoint: Int, secondPoint: Int): List[Int] = {
    val result = list.toArray
    val tmp = result(firstPoint)
    result(firstPoint) = result(secondPoint)
    result(secondPoint) = tmp
    result.toList
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
  //FUNCTIONS THAT RANDOMLY INITIALIZE THE MOVES (POSITIONS)
  def SWAP(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size) //[0,n-1]
    var secondPoint = firstPoint
    while (secondPoint == firstPoint) { //second point must be different than first
      secondPoint = Random.nextInt(list.size)
    }
    SWAPdefineMove(list, firstPoint, secondPoint)
  }
  def INV(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint) //[firstPoint+1,n]
    INVdefineMove(list, firstPoint, secondPoint)
  }
  def BckINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1)//[firstPoint+1,n]
    BckINSdefineMove(list, firstPoint, secondPoint)
  }
  def FwINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1) //[firstPoint+1,n]
    FwINSdefineMove(list, firstPoint, secondPoint)
  }
  
  def SHIFT(list: List[Int]): List[Int] = {
    val randomNo = Random.nextDouble()
    if(randomNo < 0.5)
      BckINS(list)
    else
      FwINS(list)
  }
  def INS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size) //[0,n-1]
    var secondPoint = firstPoint
    while (secondPoint == firstPoint) { //second point must be different than first
          secondPoint = Random.nextInt(list.size)
    }
    INSdefineMove(list, firstPoint, secondPoint)
  }
  //FUNCTIONS THAT RANDOMLY INITIALIZE THE MOVES (POSITIONS), AND 
  //IN ADDITION RETURN WHICH MOVE THEY USED
  def SWAPreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = Random.nextInt(list.size) //[0,n-1]
    var secondPoint = firstPoint
    while (secondPoint == firstPoint) { //second point must be different than first
          secondPoint = Random.nextInt(list.size)
    }
    val pair = (firstPoint, secondPoint)
    val result = SWAPdefineMove(list, firstPoint, secondPoint)
    (result.toList, pair)
  }
  def INVreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint) //[firstPoint+1,n]
    val pair = (firstPoint, secondPoint)
    val result = INVdefineMove(list, firstPoint, secondPoint)
    (result, pair)
  }
  def BckINSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = Random.nextInt(list.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1)//[firstPoint+1,n]
    val pair = (firstPoint, secondPoint)
    val result =  BckINSdefineMove(list, firstPoint, secondPoint)
    (result, pair)
  }
  def FwINSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1) //[firstPoint+1,n]
    val pair = (firstPoint, secondPoint)
    val result = FwINSdefineMove(list, firstPoint, secondPoint)
    (result, pair)
  }
  def SHIFTreturnMove(list: List[Int]): (List[Int], (Int,Int)) = {
    val randomNo = Random.nextDouble()
    if(randomNo < 0.5)
      BckINSreturnMove(list)
    else
      FwINSreturnMove(list)
  }
  def INSreturnMove(list: List[Int]): (List[Int], (Int, Int)) = {
    val firstPoint = Random.nextInt(list.size) //[0,n-1]
    var secondPoint = firstPoint
    while (secondPoint == firstPoint) { //second point must be different than first
          secondPoint = Random.nextInt(list.size)
    }
    val pair = (firstPoint, secondPoint)
    val el1 = list.drop(firstPoint).take(1)
    if(firstPoint < secondPoint) {//FwINS
      val result = FwINSdefineMove(list, firstPoint, secondPoint)
      (result, pair)
    }
    else {//BckINS, firstPoint > secondPoint
      //in BckINS call we need to reverse points, e.g. (5,1) becomes (1,5)
      val result = BckINSdefineMove(list, secondPoint, firstPoint)
      (result, pair)
    }
  }
}