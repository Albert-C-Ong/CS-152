
package value

import expression._
import context._

class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv) {
  
  private var cache : Value = null

  def apply(callEnv : Environment) = {
    if (cache == null) {
      cache = super.apply(List(), callEnv)
      cache
    }
    else cache
  }
}

object Thunk {
  def apply(body: Expression, env: Environment): Thunk = new Thunk(body, env)
}
