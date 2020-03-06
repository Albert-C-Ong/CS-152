
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Revision: 2020.03.05
 */

object RecognizerTests extends App with Recognizers {
  
  // exp1 ::= 00 ~ 11 | 111
  def exp1 = pipe(follows(matches("00"), matches("11")), matches("111"))
  
  println(exp1("0011")) // = true
  println(exp1("111"))  // = true
  println(exp1("000011")) // = false, too many 0's

}


