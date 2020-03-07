
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: Problems #5 Acorn
 * 
 * Revision: 2020.03.06
 */

object TestAcorn extends App {
  
  abstract class Expression {
     def execute: Double
  }

  class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
    def execute = operand1.execute + operand2.execute
    override def toString = "(+ " + operand1 + " " + operand2 + ")"
  }

  object Sum {
    def apply(operand1: Expression, operand2: Expression) =
        new Sum(operand1, operand2)
  }

  class Number(val value: Double) extends Expression {
    def execute(): Double = value
    override def toString = "Number = " + value
  }
  
   var exp: Expression = Sum(Number(42), Product(Number(3.14), Number(2.71)))
   //~ println("the value of " + exp + " = " + exp.execute)
   //~ exp = Product(Number(2), Product(Number(3), Number(5)))
   //~ println("the value of " + exp + " = " + exp.execute)
}

