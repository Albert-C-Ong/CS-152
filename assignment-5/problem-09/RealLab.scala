
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: Problem #9 - Real Lab
 * 
 * Revision: 2020.03.09
 */


class Real(val scalaValue: Double) extends Ordered[Real] with Equals {
  
  def canEqual(x: Any) = x.isInstanceOf[Real]


  override def equals(that: Any): Boolean = {
    
    that match {
      case that: Real => { this.scalaValue == that.scalaValue}
      case _ => false
    }
  }
  
  
  override def compare(that: Real): Int = {
    (this.scalaValue - that.scalaValue).toInt
  }
  
  def *(that: Real): Double = { this.scalaValue * that.scalaValue }
}


object Real {
  def apply(scalaValue: Double) = new Real(scalaValue)
}


object RealLab extends App {
  
  var r1 = Real(3.14)
  var r2 = Real(2.71)
  println("r1 * r2 = " + (r1 * r2))
  println("r1 == r2 = " + (r1 == r2))
  println("r1 < r2 = " + (r1 < r2))
}

