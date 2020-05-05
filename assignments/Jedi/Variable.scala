
package value

class Variable (var content : Value) extends Value {
  override def toString = "[" + content.toString + "]"
}

object Variable {
  def apply(content: Value) = new Variable(content)
}
