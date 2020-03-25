
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Regular Expression Lab
 * 
 * Revision: 2020.03.24
 */

object RegularExpressionLab extends App {

  //====================================================================
  // Problem #1: Natural Numbers
  //====================================================================

  val natPattern = "0|[0-9]0*"

  println("Problem #1")
  println("0".matches(natPattern))
  println("007".matches(natPattern))
  println("700".matches(natPattern))
  println()
  
  //====================================================================
  // Problem #2: Integers
  //====================================================================

  val intPattern = """0|(\+|-)?[1-9][0-9]*"""

  println("Problem #2")
  println("0".matches(intPattern))
  println("007".matches(intPattern))
  println("100".matches(intPattern))
  println("-100".matches(intPattern))
  println("+100".matches(intPattern))
  println("-0".matches(intPattern))
  println()


  //====================================================================
  // Problem #3: Floating Point Numbers
  //====================================================================
  
  val floatPattern = """(0|(\+|-)?[1-9][0-9]*)(\.[0-9]+)?|(\+|-)0\.[0-9]+"""
  
  println("Problem #3")
  println("100".matches(floatPattern))
  println("100.001".matches(floatPattern))
  println("100.".matches(floatPattern))
  println(".001".matches(floatPattern))
  println("0.001".matches(floatPattern))
  println("007".matches(floatPattern))
  println()
  
  println($SOURCE_PATH)
}
