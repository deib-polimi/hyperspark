package util

import scala.util.Random

/**
 * @author Nemanja
 */
class RNG {
  def get() =  new Random()
  def get(seed: Long) = new Random(seed)
}
object RNG {
  def apply() = {
    new RNG().get()
  }
  def apply(rngSeed: Option[Long]) = {
    rngSeed match {
      case Some(seed) => new RNG().get(seed)
      case None => new RNG().get()
    }
  }
}