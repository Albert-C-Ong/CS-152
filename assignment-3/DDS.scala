
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #3
 * Problem #8: Discrete Dynamical Systems
 * 
 * Revision: 2020.02.21
 */

object DDS extends App {

  //====================================================================
  // Problem #1: Control Loop
  //====================================================================

  def controlLoop[S](
  
    state: S, 
    cycle: Int, 
    halt: (S, Int) => Boolean, 
    update: (S, Int) => S): S = {
    
    if (halt(state, cycle)) {
      state
    }
    else {
      controlLoop(update(state, cycle), cycle + 1, halt, update)
    }
  }
  
   
  //====================================================================
  // Problem #2: Population Growth
  //====================================================================

  def amoebas(population: Int): Int = {
    
    def halt(x: Int, time: Int) = x > 100000
    def update(x: Int, time: Int) = x * 2
    
    controlLoop(population, 0, halt, update)
  }
  
  println("Problem #2: Population Growth")
  println("amoebas(1) = " + amoebas(1))
  println("amoebas(5) = " + amoebas(5))
  println("amoebas(100) = " + amoebas(100) + "\n")


  //====================================================================
  // Problem #3: Finding Roots of Functions
  //====================================================================

  def solve(f: Double => Double): Double = {
    
    val delta = 1e-8
    
    def df(x: Double) = (f(x + delta) - f(x)) / delta
    def goodEnuf(guess: Double, time: Int) = math.abs(f(guess)) <= delta
    def improve(guess: Double, time: Int) = guess - f(guess) / df(guess)
  
    controlLoop(1.0, 0, goodEnuf, improve)
  }

  //====================================================================
  // Problem #4: Approximating Square Roots
  //====================================================================


  def squareRoot(x: Double) = solve((y: Double) => y * y - x)
  
  println("Problem #4: Approximating Square Roots")
  println("squareRoot(16) = " + squareRoot(16))
  println("squareRoot(25) = " + squareRoot(25))
  println("squareRoot(100) = " + squareRoot(100) + "\n")


  //====================================================================
  // Problem #5: Approximating Cube Roots
  //====================================================================

  def cubeRoot(x: Double) = solve((y: Double) => y * y * y - x)

  println("Problem #5: Approximating Cube Roots")
  println("cubeRoot(27) = " + cubeRoot(27))
  println("cubeRoot(64) = " + cubeRoot(64))
  println("cubeRoot(700) = " + cubeRoot(700) + "\n")


  //====================================================================
  // Problem #6: Approximating Nth Roots
  //====================================================================

  def nthRoot(x: Double, n: Int) = solve((y: Double) => math.pow(y, n) - x)
  println("Problem #6: Approximating Nth Roots")
  println("nthRoot(4, 2) = " + nthRoot(4, 2))
  println("nthRoot(64, 3) = " + nthRoot(64, 3))
  println("nthRoot(1000, 5) = " + nthRoot(1000, 5) + "\n")


  //====================================================================
  // Problem #7: Compound Interest
  //====================================================================
  
  def getValue(period: Int) = {
    
    def isMature(v: Double, time: Int) = period <= time
    def compound(v: Double, time: Int) = v + (1.0 / period) * v
    
    controlLoop(1.0, 0, isMature, compound)
  }

  println("Problem #7: Compound Interest")
  println("monthly = " + getValue(12))
  println("daily = " + getValue(365))
  println("hourly = " + getValue(365 * 24))
  println("secondly = " + getValue(365 * 24 * 60 * 60))
}

