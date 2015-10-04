package it.polimi.hyperh.neighbourhood

import it.polimi.hyperh.solution.Solution
import scala.util.Random

/**
 * @author Nemanja
 */
trait SeedingStrategy {
  def divide(seed: Option[Solution], N: Int): Array[Option[Solution]]
}
class NoStrategy extends SeedingStrategy {
  override def divide(seed: Option[Solution], N: Int): Array[Option[Solution]] = {
    Array.fill(N)(None)
  }
}
class SameSeeds extends SeedingStrategy {
  override def divide(seed: Option[Solution], N: Int): Array[Option[Solution]] = {
    Array.fill(N)(seed)
  }
}
class SlidingWindow(windowSize: Int) extends SeedingStrategy {
  override def divide(seedOption: Option[Solution], N: Int): Array[Option[Solution]] = {
    val seed = seedOption.getOrElse(throw new RuntimeException("SeedingStrategySlidingWindow: None value for Option[Solution]"))
    val perm = seed.permutation
    if(N+windowSize > perm.size)
      throw new RuntimeException("SeedingStrategySlidingWindow: can't slide that much. Reason: N+windowSize  > solution.permutation.size. Try to decrease the windowSize parameter")
    var array: Array[Option[Solution]] = Array()
    for (i <- 0 until N) {
      val window = perm.drop(i).take(windowSize)
      val allowed = perm.filterNot(window.toSet)
      val arrayTake = Random.shuffle(allowed.toList).toArray
      val leftPart = arrayTake.take(i)
      val rightPart = arrayTake.drop(i)
      val newSol = leftPart ++ window ++ rightPart
      array :+= Some(Solution(newSol))
    }
    array
  }
}
class SeedPlusSlidingWindow(windowSize: Int) extends SeedingStrategy {
  override def divide(seed: Option[Solution], N: Int): Array[Option[Solution]] = {
    seed +: new SlidingWindow(windowSize).divide(seed, N-1)
  }
}