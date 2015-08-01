package util

/**
 * @author Nemanja
 */
object Timeout {
  def setTimeout(limit: Double) = { //introduce thread time here
      val expireTime = System.currentTimeMillis() + limit
      expireTime
    }
    def notTimeout(expireTimeMillis: Double): Boolean = {
      if (System.currentTimeMillis() > expireTimeMillis)
        false
      else true
    }
}