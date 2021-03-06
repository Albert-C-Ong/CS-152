
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Revision: 2000.00.00
 */

import scala.util.parsing.combinator._

object testing extends App {
  
  def symbol: Parser[Any] = "+" | "-" | "*"

  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
}
