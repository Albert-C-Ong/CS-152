
package value

import expression._
import context._

case class Real(val value: Double) extends Literal with Ordered[Real] with Equals {
  
  // Add, multiple, subtract, and division operators
  def +(other: Real) = Real(this.value + other.value)
  def *(other: Real) = Real(this.value * other.value)
  def -(other: Real) = Real(this.value - other.value)
  def /(other: Real) = Real(this.value / other.value)
  
  def unary_- = Real(-1.0 * value) // unary negation
  override def toString = value.toString
  def compare(other: Real): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Integer]
  override def equals(other: Any): Boolean =
    other match {
       case other: Real => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}

object Real {
  implicit def realToInteger(n: Real): Integer = Integer(n.value.toInt)
}
