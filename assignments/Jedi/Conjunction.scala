
package expression

import value._
import context._

case class Conjunction(val ops: List[Expression]) extends SpecialForm {
  
  def execute(env: Environment): Value = {
    var i = 0
    var result = true
    
    while (i < ops.size && result) {
      
      val executedOp = ops(i).execute(env)
      
      if (!executedOp.isInstanceOf[Boole])
        throw new TypeException("Conjunction requires type Boole only")
      else if (!executedOp.asInstanceOf[Boole].value)
        result = false
      else i = i + 1
    }
    
    Boole(result)
  }
}
