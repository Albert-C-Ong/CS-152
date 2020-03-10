
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: Problems #5 - Acorn
 * 
 * Revision: 2020.03.09
 */

// Expression class (given)
abstract class Expression {
   def execute: Double
}


// Sum class and object (given)
class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
  def execute = operand1.execute + operand2.execute
  override def toString = "(+ " + operand1 + " " + operand2 + ")"
}

object Sum {
  def apply(operand1: Expression, operand2: Expression) =
      new Sum(operand1, operand2)
}


// Number class and object
class Number(val value: Double) extends Expression {
  def execute(): Double = value
  override def toString = value.toString
}

object Number {
  def apply(value: Double) = new Number(value)
}


// Product class and object
class Product(val a: Expression, val b: Expression) extends Expression {
  def execute(): Double = a.execute * b.execute
  override def toString = "(* " + a + " " + b + ")"
}

object Product {
  def apply(a: Expression, b: Expression) = 
    new Product(a, b)
}


// Test case
object TestAcorn extends App {
  var exp: Expression = Sum(Number(42), Product(Number(3.14), Number(2.71)))
  println("the value of " + exp + " = " + exp.execute)
  exp = Product(Number(2), Product(Number(3), Number(5)))
  println("the value of " + exp + " = " + exp.execute)
}

