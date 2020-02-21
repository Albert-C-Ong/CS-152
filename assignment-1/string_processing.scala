

/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #1: string processing
 * 
 * Revision: 2020.02.07
 */

import scala.util.Random
import scala.collection.immutable.StringOps

object string_processing extends App {
  
  
  //====================================================================
  // Problem #1
  //====================================================================
  
  def isPal(str: String): Boolean = {
    str == str.reverse
  }
  
  // Test cases
  //~ println("Problem #1")
  //~ println("isPal('rotator') = " + isPal("rotator"))
  //~ println("isPal('cat') = " + isPal("cat"))
  //~ println("isPal('Civis') = " + isPal("Civic"))
  //~ println("isPal('Toyota') = " + isPal("Toyota"))
  //~ println("isPal('$3.1441.3$') = " + isPal("$3.1441.3$") + "\n")
  
  
  //====================================================================
  // Problem #2
  //====================================================================
  
  def isPal2(str: String): Boolean = {
	
	var x = str.replaceAll("[\p{Punct}&&[^.]]", "")
	
	println(x)
	
	true
  }

  println(isPal2("Hello! xxx"))

  //~ println("Problem #3")
  //~ println("isPal2('A man, a plan, a canal, Panama!')  = " + 
           //~ isPal2("A man, a plan, a canal, Panama!") + "\n")
  
  //====================================================================
  // Problem #3
  //====================================================================
  
  def mkWord(size: Int = 5) = {
	
	var rand_word = ""
	
	for (x <- 0 to size) {
	  
	  var rand_int = Random.nextInt(26) + 97
	  var rand_char = rand_int.toChar
	  
	  rand_word += rand_char
	  
	}
	
	rand_word
  }
  
  //~ println("Problem #3")
  //~ println("mkWord() = " + mkWord())
  //~ println("mkWord() = " + mkWord())
  //~ println("mkWord() = " + mkWord())
  //~ println("mkWord(20) = " + mkWord(20) + "\n")
  
  
  //====================================================================
  // Problem #4
  //====================================================================
  
  def mkSentence(size: Int = 10) = {
	  
	  var sentence = ""
	  
	  for (x <- 0 to size) {
	    
	    var rand_word = mkWord(Random.nextInt(20))
	    
	    if (x != 0) {
		  rand_word = " " + rand_word;
		}
	    
	    sentence += rand_word
	    
	  }
	  
	  sentence.capitalize + "."
  }
  
  //~ println(mkSentence())
  //~ println(mkSentence())
  //~ println(mkSentence())
  //~ println(mkSentence())
  //~ println(mkSentence())
  
}
