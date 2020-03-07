
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: Problem #9 - Real Lab
 * 
 * Revision: 2020.03.06
 */

object RealLab extends App {
  
  class Real(val scalaValue: Double) extends Ordered[Real] with Equals {
  
    override def equals(other: Real): Boolean = {
      scalaValue == other.scalaValue
    }
  
  }
  
  var r1 = Real(3.14)
  var r2 = Real(2.71)
  println("r1 * r2 = " + (r1 * r2))
  println("r1 == r2 = " + (r1 == r2))
  println("r1 < r2 = " + (r1 < r2))
}

