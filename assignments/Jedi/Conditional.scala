
package expression

import value._
import context._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
      val value = condition.execute(env)
      if(!value.isInstanceOf[Boole]) throw new TypeException("Condition must be of type Boole, got: " + value.getClass)
        val valueData = value.asInstanceOf[Boole].value
      if(valueData) consequent.execute(env)
      else if(null != alternative) alternative.execute(env)
      else Notification.UNSPECIFIED
    }
}
