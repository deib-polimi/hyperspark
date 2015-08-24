package it.polimi.hyperh.search

import scala.util.Random

/**
 * @author Nemanja
 */
object NeighbourhoodSearch {
  
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
  }
  def INV(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint) //[firstPoint+1,n]
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(firstPoint).take(secondPoint - firstPoint).reverse
    val resultPart3 = list.drop(secondPoint)
    resultPart1 ::: resultPart2 ::: resultPart3
  }
  def BckINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1)//[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1)//[firstPoint+1,n]
    val resultPart1 = list.take(firstPoint)
    val resultPart2 = list.drop(secondPoint).take(1)
    val resultPart3 = list.drop(firstPoint).filterNot(resultPart2.toSet)
    val result = resultPart1 ::: resultPart2 ::: resultPart3
    result
  }
  def FwINS(list: List[Int]): List[Int] = {
    val firstPoint = Random.nextInt(list.size - 1) //[0,n-2]
    val secondPoint = firstPoint + 1 + Random.nextInt(list.size - firstPoint - 1) //[firstPoint+1,n]
    val el1 = list.drop(firstPoint).take(1)
    val resultPart1 = list.take(secondPoint+1).filterNot(el1.toSet)
    val resultPart2 = list.drop(secondPoint+1)
    val result = resultPart1 ::: el1 ::: resultPart2
    result
  }
  
  def SHIFT(list: List[Int]): List[Int] = {
    val randomNo = Random.nextDouble()
    if(randomNo < 0.5)
      BckINS(list)
    else
      FwINS(list)
  }
}