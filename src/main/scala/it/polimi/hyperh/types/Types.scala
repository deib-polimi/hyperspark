package it.polimi.hyperh.types

object Types {

   type Permutation = Array[Int] 
   type Constraint = (Int => Boolean)
   type Constraints = Array[Constraint] 
   type Value = Int
}