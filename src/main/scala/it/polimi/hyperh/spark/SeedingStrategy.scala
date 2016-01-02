package it.polimi.hyperh.spark

import it.polimi.hyperh.solution.Solution
import scala.util.Random

/**
 * @author Nemanja
 */
trait SeedingStrategy extends Serializable {
  def divide(seed: Option[Solution], N: Int): Array[Option[Solution]]
  def usesTheSeed(): Boolean
}
class NoStrategy extends SeedingStrategy {
  override def divide(seed: Option[Solution], N: Int): Array[Option[Solution]] = {
    Array.fill(N)(None)
  }
  override def usesTheSeed(): Boolean = false
}
class SameSeeds extends SeedingStrategy {
  override def divide(seed: Option[Solution], N: Int): Array[Option[Solution]] = {
    Array.fill(N)(seed)
  }
  override def usesTheSeed(): Boolean = true
}
