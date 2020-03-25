package value

import expression._

case class Boole(val value: Boolean) extends Literal with Equals {
  def unary_! = Boole(!(this.value))
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
}
