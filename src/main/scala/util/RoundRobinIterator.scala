package util

/**
 * @author Nemanja
 */
class RoundRobinIterator(size: Int) {
  val circular = Iterator.continually((0 until size)).flatten
  def next() = circular.next()
}