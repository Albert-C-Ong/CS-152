
package expression

import value._
import context._

case class Disjunction(cond1: Expression, cond2: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    val ex_cond1 = cond1.execute(env)
    val ex_cond2 = cond2.execute(env)
    
    Boole(ex_cond1.asInstanceOf[Boole].value && ex_cond2.asInstanceOf[Boole].value)
  }
}

