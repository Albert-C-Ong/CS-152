
package expression

import value._
import context._

class Lambda(val params : List[Identifier], 
             val body : Expression) extends SpecialForm {
               
  def execute(env : Environment) = new Closure(params, body, env)
}

object Lambda {
  def apply(params : List[Identifier], body : Expression) = 
  new Lambda(params, body)
}
