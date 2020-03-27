package value

import expression._

case class Chars(val value: String) extends Literal with Ordered[Chars] with Equals {
  
  def +(other: Chars) = Chars(this.value + other.value)
  
  override def toString = value.toString
  
  def substring(a: Integer, b: Integer) = Chars(this.value.substring(a.value, b.value))
  
  def compare(other: Chars): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  
  override def canEqual(other: Any) =  other.isInstanceOf[Chars]
  
  override def equals(other: Any): Boolean =
    other match {
       case other: Chars => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
}

//~ object Chars {
  //~ def apply(value: String) = new Chars(value)
//~ }
