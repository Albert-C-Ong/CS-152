
trait Expression {
  def execute: Double
}

case class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
  def execute =
    if (operand2 == null) operand1.execute
    else operand1.execute + operand2.execute
}

case class Product(val operand1: Expression, val operands: List[Expression]) extends Expression {
  def execute =
    if (operands == Nil) operand1.execute
    else operand1.execute * operands.map(_.execute).reduce(_*_)
}

case class Number(val value: Double) extends Expression {
  def execute = value
}
