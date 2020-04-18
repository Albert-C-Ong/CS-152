
package expression

import value._
import context._

// for example: def x = add(2, 3)
case class Declaration(val iden: Identifier, val exp: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    val result = exp.execute(env)
    env.put(iden, result)
    Notification.OK
  }
}

