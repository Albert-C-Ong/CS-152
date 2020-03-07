
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: problem #2
 * 
 * Revision: 2020.03.06
 */


object GladiatorLab extends App {
  
  
  class Gladiator(val name: String) {
    
    var health = 100; 
    
    def damage(x: Int) {
      health -= x
      if (health < 0) health = 0
      println(name + "'s health = " + health)
    }
    
    
    def attack(other: Gladiator) {
      
      if (health > 0) {
        println("Attacker: " + name)
        println("Victim: " + other.name)
        
        other.damage(scala.util.Random.nextInt(health))
      }
      else {
        println("Dead gladiators don't fight!")
      }
      
      println()
    }
  }
  
  
  trait Slasher {
    def slash(other: Gladiator) { println("Slash!"); other.damage(5) }
  }
  
  trait Masher {
    def mash(other: Gladiator) { println("Mash!"); other.damage(5) }
  }
  
  trait Crusher {
    def crush(other: Gladiator) { println("Crush!"); other.damage(5) }
  }
  
  
  class CrusherMasher(name: String) extends Gladiator(name) with Crusher with Masher {
    
    override def attack(other: Gladiator) {
      
      if (health > 0) {
        println("Attacker: " + name)
        println("Victim: " + other.name)
        
        crush(other)
        mash(other)
      }
      else {
        println("Dead gladiators don't fight!")
      }
      println()
    }
  }
  
  //====================================================================
  
  // Instantiating Optimus Prime
  var maximus = new CrusherMasher("Optimus Prime")
  
  // Instantiating Bumble Bee
  var bee = new Gladiator("Bumble Bee") with Slasher with Masher {
    
    override def attack(other: Gladiator) { 
      
      if (health > 0) {
        println("Attacker: " + name)
        println("Victim: " + other.name)
        
        slash(other)
        mash(other)
      }
      else {
        println("Dead gladiators don't fight!")
      }
      
      println()
    }
  }
  
  // Test code
  for(i <- 0 to 5) {
     maximus.attack(bee)
     bee.attack(maximus)
   }   
}

