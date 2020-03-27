
import scala.util.parsing.combinator._

class SOPParsers extends RegexParsers {
  // EXPRESSION ::= SUM
  def expression: Parser[Any] = sum
  // SUM ::= PRODUCT~(+~SUM)?
  def sum: Parser[Any] = product ~ opt("+" ~ sum)
  // PRODUCT ::= TERM~(*~TERM)*
  def product: Parser[Any] = term ~ rep("*" ~ term)
  // TERM ::= NUMBER | (SUM)
  def term: Parser[Any] = number | "(" ~ expression ~ ")"
  // NUMBER ::= [0-9]+(.[0-9]+)?
  def number: Parser[Any] = """0|[1-9][0-9]*(\.[0-9]+)?""".r
}

