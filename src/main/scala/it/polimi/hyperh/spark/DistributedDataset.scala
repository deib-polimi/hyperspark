package it.polimi.hyperh.spark

import it.polimi.hyperh.algorithms.Algorithm
import it.polimi.hyperh.solution.Solution

/**
 * @author Nemanja
 */
class DistributedDatum(alg: Algorithm, seedOption: Option[Solution], iterTimeLimit: Double) extends Serializable {
  def algorithm = alg
  def seed = seedOption
  def iterationTimeLimit = iterTimeLimit
}
object DistributedDatum {
  def apply(algorithm: Algorithm, seed: Option[Solution], iterationTimeLimit: Double) =  {
    new DistributedDatum(algorithm, seed, iterationTimeLimit)
  }
}
object DistributedDataset {
  def apply(numOfNodes: Int, algorithms: Array[Algorithm], seeds: Array[Option[Solution]], iterationTimeLimit: Double) =  {
    var array: Array[DistributedDatum] = Array()
    for(i <- 0 until numOfNodes) {
      val datum = DistributedDatum(algorithms(i), seeds(i), iterationTimeLimit)
      array :+= datum
    }
    array
  }
}