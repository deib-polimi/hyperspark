package util
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

/**
 * @author Nemanja
 */
object Timeout {
  
  private def getThreadTime() = {
    val threadTimeNanos = ManagementFactory.getThreadMXBean().getThreadCpuTime(Thread.currentThread().getId());
    val threadTimeMillis = threadTimeNanos / 1000000
    threadTimeMillis
  }
  def setTimeout(limit: Double) = {
      val threadTimeMillis = getThreadTime()
      val expireTimeMillis = threadTimeMillis + limit
      expireTimeMillis
    }
    def notTimeout(expireTimeMillis: Double): Boolean = {
      val threadTimeMillis = getThreadTime()
      if (threadTimeMillis > expireTimeMillis)
        false
      else true
    }
}