
package expression

import value._
import context._

case class Disjunction(val ops: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    try {
        for (op <- ops) {
          if (op.execute(env).asInstanceOf[Boole].value)
            return Boole(true)
        }
        Boole(false)
      } 
      catch {
        case e: TypeException => throw new TypeException("Operands must be of type Boole")
    }
  }
}

