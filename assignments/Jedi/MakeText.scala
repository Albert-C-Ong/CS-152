
package expression

import value._
import context._

class MakeText(body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = Text(body)
}

object MakeText {
  def apply(body: Expression): MakeText = new MakeText(body)
}

