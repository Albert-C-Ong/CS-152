
package expression 

import expression.Expression
import context.Environment

case class Identifier(val name: String) extends Expression {
   override def toString = name
   def execute(env: Environment) = name
}
