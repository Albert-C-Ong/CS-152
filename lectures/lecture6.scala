
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Revision: 2020.03.05
 */

object lecture6 extends App {

  var test = "Hello world!"

  for (i <- 0 to test.size) {
    println(test.take(i) + " " + test.drop(i))
  
  }
}

