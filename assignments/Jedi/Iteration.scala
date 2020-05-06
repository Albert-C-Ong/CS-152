
package expression

import context._
import value._

// while(condition) body
class Iteration(val condition: Expression, val body: Expression) extends SpecialForm {
 
  override def execute(env: Environment): Value = {
    
    var res = condition.execute(env)
    
    if (!res.asInstanceOf[Boole].value) throw new TypeException("Condition of while must be a Boole")
    
    var cond = (res.asInstanceOf[Boole]).value
    
    while (cond) {
      body.execute(env)
    }
    
    Notification.DONE
  }
}

object Iteration {
  def apply(condition: Expression, body: Expression) = new Iteration(condition, body)
}
