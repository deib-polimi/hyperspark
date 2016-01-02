package it.polimi.hyperh.spark

import it.polimi.hyperh.algorithms.Algorithm
import it.polimi.hyperh.solution.Solution

/**
 * @author Nemanja
 */
class DistributedDatum(alg: Algorithm, seedOption: Option[Solution], stopCond: StoppingCondition) extends Serializable {
  def algorithm = alg
  def seed = seedOption
  def stoppingCondition = stopCond
}
object DistributedDatum {
  def apply(algorithm: Algorithm, seed: Option[Solution], stopCond: StoppingCondition) =  {
    new DistributedDatum(algorithm, seed, stopCond)
  }
}
object DistributedDataset {
  def apply(numOfNodes: Int, algorithms: Array[Algorithm], seeds: Array[Option[Solution]], stopCond: StoppingCondition) =  {
    var array: Array[DistributedDatum] = Array()
    for(i <- 0 until numOfNodes) {
      val datum = DistributedDatum(algorithms(i), seeds(i), stopCond)
      array :+= datum
    }
    array
  }
}