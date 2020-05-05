
package value

import expression._
import context._

class Closure(val params: List[Identifier], 
              val body: Expression,
              val defEnv: Environment) extends Value {

  def apply(args: List[Value], callEnv: Environment = null): Value = {
    
    if (params.size != args.size)
      throw new TypeException("Args and params are not the same size")
      
    var temp: Environment = null
    
    if (Flags.useStaticScopeRule) {
      temp = new Environment(defEnv)
    }
    else {
      temp = new Environment(callEnv)
    }
    
    temp.bulkPut(params, args)
    body.execute(temp)
  }
}
