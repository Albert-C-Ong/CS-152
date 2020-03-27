
import scala.util.parsing.combinator._

class SOP2Parsers extends RegexParsers {
  def expression: Parser[Expression] = sum
  def sum: Parser[Expression] = product ~ opt("+" ~ sum) ^^ {
    case p ~ None => p
    case p ~ Some("+" ~ s) => Sum(p, s)
  }
  def product: Parser[Expression] = term ~ rep("*" ~> term) ^^ {
    case t ~ Nil => t
    case t ~ terms => Product(t, terms)
  }
  def term: Parser[Expression] = number | "(" ~> expression <~ ")"
  def number: Parser[Number] = """0|[1-9][0-9]*(\.[0-9]+)?""".r ^^ {
    case num => Number(num.toDouble)
  }
}
