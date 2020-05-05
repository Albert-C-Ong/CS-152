
package expression

import context._
import expression._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression] = Nil) extends Expression {
  
  def execute(env: Environment): Value = {
    
    //~ val args: List[Value] = operands.map(_.execute(env))
    //~ alu.execute(operator, operands)
    
    try {
      val maybeClosure = operator.execute(env)
      if (!maybeClosure.isInstanceOf[Closure])
        throw new UndefinedException(operator)
      else {
        val args: List[Value] = freeze(operands, env)
        maybeClosure.asInstanceOf[Closure].apply(args, env)
      }
    } 
    catch {
      case e: UndefinedException => alu.execute(operator, operands.map(_.execute(env)))
    }
  }
  
  
  def freeze(ops: List[Expression], env: Environment) : List[Value] = {
    if (Flags.paramaterPassing == Flags.passByName) ops.map(new Thunk(_, env))
    else if (Flags.paramaterPassing == Flags.passByText) ops.map(Text(_))
    else ops.map(_.execute(env))
  }
}

