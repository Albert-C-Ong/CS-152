
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #3
 * Problem #8: Discrete Dynamical Systems
 * 
 * Revision: 2020.02.18
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
  
  // def update(currentState: S, cycle: Int) = the next state
  
  //~ def halt(currentState: S, cycle: Int) =
   //~ if (currentState is final?) true else false
   
  //====================================================================
  // Problem #2: Population Growth
  //====================================================================


  //====================================================================
  // Problem #3: Finding Roots of Functions
  //====================================================================

  //====================================================================
  // Problem #4: Approximating Square Roots
  //====================================================================


  //====================================================================
  // Problem #5: Approximating Cube Roots
  //====================================================================


  //====================================================================
  // Problem #6: Approximating Nth Roots
  //====================================================================


  //====================================================================
  // Problem #7: Compound Interest
  //====================================================================
  
  def getValue(period: Int) = {
    
    def isMature(v: Double, time: Int) = period <= time
    def compound(v: Double, time: Int) = v + (1.0 / period) * v
    
    controlLoop(1.0, 0, isMature, compound)
  }

  //~ println("Problem #7: Compound Interest")
  //~ println("monthly = " + getValue(12))
  //~ println("daily = " + getValue(365))
  //~ println("hourly = " + getValue(365 * 24))
  //~ println("secondly = " + getValue(365 * 24 * 60 * 60))
  
  
  //====================================================================
  // ex
  //====================================================================
  
  def solve(f: Double => Double): Double = {
    
    val delta = 1e-8
    
    def df(x: Double) = (f(x + delta) - f(x)) / delta
    def goodEnuf(guess: Double, time: Int) = math.abs(f(guess)) <= delta
    def improve(guess: Double, time: Int) = guess - f(guess) / df(guess)
  
    controlLoop(1.0, 0, goodEnuf, improve)
  }
  
  
  def f(x: Double) = solve((x: Double) => 2 * x + 5)
  println(f(10))
  
  def cubeRoot(x: Double) = solve((y: Double) => y * y * y - x)
  
  println(cubeRoot(27))
  println(cubeRoot(64))
  
}

