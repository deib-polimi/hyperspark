package it.polimi.hyperh

object Types {

   type Permutation = Array[Int] 
   type Constraint = (Int => Boolean)
   type Constraints = Array[Constraint] 
   type Value = Int
}