
package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {
  
  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params : Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case Some(id ~ Nil)  => List(id)
    case Some(id ~ more) => id :: more
    case _               => List()
  }
  
  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda : Parser[Expression] = "lambda" ~> params ~ expression ^^ {
    case param ~ expression => Lambda(param, expression)
  }
  
  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block : Parser[Block] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case exp ~ Nil  => Block(List(exp))
    case exp ~ rest => Block(exp :: rest)
  }
  
  
  // override of term parser
  override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
}

