package util

import it.polimi.hyperh.solution.EvaluatedSolution

/**
 * @author Nemanja
 */
object Performance {
  def RPD(someVal: Int, bestVal: Int): Double = {
    val diff = someVal - bestVal
    if(diff < 0)
      println("New best :" + someVal)
    BigDecimal(100 * diff / bestVal.toDouble).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  def RPD(Csome: EvaluatedSolution, Cbest: EvaluatedSolution): Double = {
    val someVal = Csome.value
    val bestVal = Cbest.value
    val diff = someVal - bestVal
    if(diff < 0)
      println("New best :" + Csome)
    BigDecimal(100 * diff / bestVal.toDouble).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  def ARPD(RPDs: List[Double]): Double = {
    var sum = 0.0
    for(i <- 0 until RPDs.size){
      sum = sum + RPDs(i)
    }
    val arpd = sum / RPDs.size
    arpd
  }
    
}