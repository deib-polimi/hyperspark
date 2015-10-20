package util

/**
 * @author Nemanja
 */
class RoundRobinIterator(size: Int) extends Serializable {
  protected val array: Array[Int] = (0 until size).toArray
  protected var index: Int = -1
  def next(): Int = {
    index = (index + 1) % size
    array(index)
  }
}