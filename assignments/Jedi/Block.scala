
package expression

import context._
import value._

case class Block(var exps : List[Expression]) extends SpecialForm { 
  
  def execute(env : Environment) : Value = {
    
    var temp = new Environment(env)
    var result : Value = null
    
    for(exp <- exps) {
      result = exp.execute(temp)
    }
    
    result
  }
}
