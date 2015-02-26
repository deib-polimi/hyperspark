package it.polimi.hyperh

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  (List(1,8), List(5,5)).zipped.map(Math.max(_,_))//> res0: List[Int] = List(5, 8)
  
  
}