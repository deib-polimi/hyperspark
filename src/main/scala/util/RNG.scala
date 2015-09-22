package util

import scala.util.Random
import scala.collection.generic.CanBuildFrom

/**
 * @author Nemanja
 */
class RNG() extends Serializable {
  private var self: Random = new Random()
  def this(seed: Long) = {
    this()
    self = new Random(seed)
  }
  def this(seed: Int) = this(seed.toLong)
  
  /** Returns the next pseudorandom, uniformly distributed boolean value
   *  from this random number generator's sequence.
   */
  def nextBoolean(): Boolean = self.nextBoolean()

  /** Generates random bytes and places them into a user-supplied byte
   *  array.
   */
  def nextBytes(bytes: Array[Byte]) { self.nextBytes(bytes) }

  /** Returns the next pseudorandom, uniformly distributed double value
   *  between 0.0 and 1.0 from this random number generator's sequence.
   */
  def nextDouble(): Double = self.nextDouble()

  /** Returns the next pseudorandom, uniformly distributed float value
   *  between 0.0 and 1.0 from this random number generator's sequence.
   */
  def nextFloat(): Float = self.nextFloat()

  /** Returns the next pseudorandom, Gaussian ("normally") distributed
   *  double value with mean 0.0 and standard deviation 1.0 from this
   *  random number generator's sequence.
   */
  def nextGaussian(): Double = self.nextGaussian()

  /** Returns the next pseudorandom, uniformly distributed int value
   *  from this random number generator's sequence.
   */
  def nextInt(): Int = self.nextInt()

  /** Returns a pseudorandom, uniformly distributed int value between 0
   *  (inclusive) and the specified value (exclusive), drawn from this
   *  random number generator's sequence.
   */
  def nextInt(n: Int): Int = self.nextInt(n)

  /** Returns the next pseudorandom, uniformly distributed long value
   *  from this random number generator's sequence.
   */
  def nextLong(): Long = self.nextLong()

  /** Returns a pseudorandomly generated String.  This routine does
   *  not take any measures to preserve the randomness of the distribution
   *  in the face of factors like unicode's variable-length encoding,
   *  so please don't use this for anything important.  It's primarily
   *  intended for generating test data.
   *
   *  @param  length    the desired length of the String
   *  @return           the String
   */
  def nextString(length: Int) = self.nextString(length)

  /** Returns the next pseudorandom, uniformly distributed value
   *  from the ASCII range 33-126.
   */
  def nextPrintableChar(): Char = self.nextPrintableChar()

  def setSeed(seed: Long) { self.setSeed(seed) }

  /** Returns a new collection of the same type in a randomly chosen order.
   *
   *  @return         the shuffled collection
   */
  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    self.shuffle(xs)(bf)
  }
  def alphanumeric: Stream[Char] = self.alphanumeric
}
object RNG {
  def apply() = {
    new RNG()
  }
  def apply(seed: Long) = {
    new RNG(seed)
  }
  def apply(seed: Int) = {
    new RNG(seed)
  }
}