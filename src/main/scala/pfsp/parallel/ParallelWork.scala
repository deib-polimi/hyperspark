package pfsp.parallel;

import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.solution.EvaluatedSolution
import pfsp.problem.PfsProblem
import scala.util.Random
import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.routing.RoundRobinPool
import pfsp.solution.PfsSolution
import pfsp.solution.PfsEvaluatedSolution

object ParallelWork extends App {
  override def main(args: Array[String]) {
    
    def calculate(p:PfsProblem, evOldSolution:PfsEvaluatedSolution, nrOfWorkers: Int, sizeOfNeighbourhood: Int) {
      // Create an Akka system
      val system = ActorSystem("ParallelSystem")

      // create the result listener, which will print the result and 
      // shutdown the system
      val listener = system.actorOf(Props[Listener], name = "listener")

      // create the master
      val master = system.actorOf(Props(new Master(p, evOldSolution, nrOfWorkers, sizeOfNeighbourhood, listener)),
        name = "master")

      // start the calculation
      master ! Calculate
    }
    val p = PfsProblem.fromResources("inst_ta001.txt")
    val permutationList = Random.shuffle(p.jobs.toList)
    val oldSolution = PfsSolution(permutationList)
    var evOldSolution = p.evaluate(oldSolution).asInstanceOf[PfsEvaluatedSolution]
    calculate(p, evOldSolution, 7, 300)
  }
  case object Calculate
  case class Work(p: PfsProblem, solution: PfsSolution, initEndTimesMatrix: Array[Array[Int]])
  case class SingleResult(evSolution: EvaluatedSolution)
  case class FinalResult(evSolution: EvaluatedSolution, startMillis: Long)
}
class Worker extends Actor {
  import ParallelWork._
  def receive = {
    case Work(p, solution, initEndTimesMatrix) =>
      val evSolution = p.evaluatePartialSolution(solution.permutation)
      sender ! SingleResult(evSolution)
  }
}
class Listener extends Actor {
  import ParallelWork._
  override def receive = {
    case FinalResult(evSolution, duration) =>
      println("bestSolution: " + evSolution + " millis: " + duration)
      context.system.shutdown()
  }
}
class Master(p: PfsProblem,
             evOldSolution: PfsEvaluatedSolution,
             nrOfWorkers: Int,
             sizeOfNeighbourhood: Int,
             listener: ActorRef) extends Actor {
  import ParallelWork._
  var nrOfResults: Int = 0
  val startMillis: Long = System.currentTimeMillis
  val initEndTimesMatrix = p.jobsInitialTimes()
  var bestSolution: EvaluatedSolution = evOldSolution
  val workerRouter = context.actorOf(
    Props[Worker].withRouter(RoundRobinPool(nrOfWorkers)), name = "workerRouter")

  override def receive = {
    case Calculate =>
      for (i <- 0 until sizeOfNeighbourhood)
        workerRouter ! Work(p, PfsSolution(Random.shuffle(p.jobs.toList)), initEndTimesMatrix)
    case SingleResult(evNewSolution) =>
      nrOfResults += 1
      bestSolution = List(evNewSolution, bestSolution).min
      if (nrOfResults == sizeOfNeighbourhood) {
        // Send the result to the listener
        listener ! FinalResult(bestSolution, System.currentTimeMillis - startMillis)
        // Stops this actor and all its supervised children
        context.stop(self)
      }
  }
}
