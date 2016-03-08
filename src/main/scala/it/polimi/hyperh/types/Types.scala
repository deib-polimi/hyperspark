package it.polimi.hyperh.types

object Types {
  type Permutation = Array[Int]
  type Constraint = (Int => Boolean)
  type Constraints = Array[Constraint]
  type Value = Int

  def Permutation(x: Int, xs: Int*): Array[Int] = {
    val array = new Array[Int](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }
}