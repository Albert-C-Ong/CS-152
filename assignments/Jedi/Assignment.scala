
package expression

import context._
import value._

class Assignment (var vbl : Identifier, var exp : Expression) extends SpecialForm {
  
  override def execute(env: Environment) : Value = {
    if(env(vbl).isInstanceOf[Variable]) {
      env(vbl).asInstanceOf[Variable].content = exp.execute(env)    
      Notification.DONE
    }
    else throw new TypeException("Contents of vbl must be of type Variable")
  }
}

object Assignment {
  def apply(vbl : Identifier, exp : Expression) = new Assignment(vbl , exp)  
}
