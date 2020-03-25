
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #1
 * mathematics
 * 
 * Revision: 2020.02.07
 */

import scala.util.Random

object mathematics extends App {

  def rollDice() = {
    (Random.nextInt(6)+ + 1, Random.nextInt(6) + 1)
  }
  
  for (x <- 0 to 5) {
    println(rollDice())
  }
}


