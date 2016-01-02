package it.polimi.hyperh.spark

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

/**
 * @author Nemanja
 */
abstract class StoppingCondition extends Serializable {
  def isSatisfied(): Boolean
  def isNotSatisfied(): Boolean = { ! isSatisfied() }
}

class TimeExpired(timeLimitMillis: Double) extends StoppingCondition {
  private var internalLimit: Double = 9999999999999999.0
  
  private def getThreadTime(): Long = {
    val threadTimeNanos = ManagementFactory.getThreadMXBean().getThreadCpuTime(Thread.currentThread().getId());
    val threadTimeMillis = threadTimeNanos / 1000000
    threadTimeMillis
  }
  def initialiseLimit() = {
    val threadTimeMillis = getThreadTime()
    val expireTimeMillis = threadTimeMillis + timeLimitMillis
    internalLimit = expireTimeMillis
    this
  }
  override def isSatisfied(): Boolean = {
    val threadTimeMillis = getThreadTime()
    if (threadTimeMillis > internalLimit)
      true
    else false
  }
  def getLimit(): Double = { timeLimitMillis }
}