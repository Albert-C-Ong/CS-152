
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: problem #2
 * 
 * Revision: 2020.03.05
 */


object gladiator extends App {
  
  println("Hello world!")
  
  class Gladiator(val name: String) {
    
    var health = 100; 
    
    def damage(x: Int) {
      health -= x
    }
  }
  
  
  class Knight(name: String, val strength: Int) extends Gladiator(name)
  
}

