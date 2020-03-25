
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Revision: 2020.03.17
 */

object midterm extends App {
  
  //====================================================================
  // Problem #1: Count Roots
  //====================================================================
  
  def countRoots[T](f: T => Double, inputs: List[T]): Int = {
    inputs.filter(f(_) == 0).size
  }
  
  
  def poly(x: Double) = (x + 1) * (x - 1) 
  
  // Test case
  println("Problem #1: ")
  println("countRoots(poly _, List(-1.0, 0.0, 1.0, 2.0)) = " + 
           countRoots(poly _, List(-1.0, 0.0, 1.0, 2.0)) + "\n")
  
  
  //====================================================================
  // Problem #2: Combinator
  //====================================================================
  
  // recur combinator
  def recur(base: Int, f: Int => Int): Int => Int = {
    
    def g(n: Int): Int = {
      
      if (n == 0) {
        base
      }
      else {
        f(g(n - 1))
      }
    }
    
    g _
  }
  
  // Original hierarchy
  //~ def double(n: Int): Int = if (n == 0) 0 else 2 + double(n - 1)
  //~ def exp(n: Int): Int = if(n == 0) 1 else double(exp(n - 1))
  //~ def hyperExp(n: Int): Int = if(n == 0) 1 else exp(hyperExp(n - 1))
  //~ def hyper2Exp(n: Int): Int = if(n == 0) 1 else exp(hyperExp(n - 1))
  
  // Redefined hierarchy
  def double = recur(0, n => n + 2)
  def exp = recur(1, double)
  def hyperExp = recur(1, exp)
  def hyper2Exp = recur(1, exp)
  
  println("Problem #2: ")
  println("double(3) = " + double(3))
  println("exp(3) = " + exp(3))
  println("hyperExp(3) = " + hyperExp(3))
  println("hyper2Exp(3) = " + hyper2Exp(3) + "\n")
  
  
  //====================================================================
  // Problem #3: List Processing
  //====================================================================
  
  // Flight class
  class Flight(val flightNo: Int, 
               val eta: Int, 
               val ata: Int, 
               val cancelled: Boolean = false) {
    
    // Throws exceptions for non-negative inputs
    if (flightNo < 0) {
      throw new Exception("flightNo must be non-negative")
    }
    else if (eta < 0) {
      throw new Exception("eta must be non-negative")
    }
    else if (ata < 0) {
      throw new Exception("ata must be non-negative")
    }
    
    // Equals method
    override def equals(other: Any) =
      other.isInstanceOf[Flight] &&
      other.asInstanceOf[Flight].flightNo == this.flightNo &&
      other.asInstanceOf[Flight].eta == this.eta &&
      other.asInstanceOf[Flight].ata == this.ata &&
      other.asInstanceOf[Flight].cancelled == this.cancelled
    
    // toString and hashCode method
    override def toString = "Flight #" + flightNo
    override def hashCode = this.toString.hashCode
  }
  
  // Flight object
  object Flight {
    def apply(flightNo: Int, eta: Int, ata: Int, cancelled: Boolean = false) =
      new Flight(flightNo, eta, ata, cancelled)
  }
  
  // avgDelay method
  def avgDelay(flights: List[Flight]): Double = {
    flights.map(x => x.ata - x.eta).reduce((a, b) => a + b) / flights.size.toDouble
  }
  
  // Test case
  val flightsMarch3 =
  List(Flight(123, 0, 30), Flight(234, 60, 65),
     Flight(345, 200, 200, true), Flight(456, 420, 510))
  
  println("Problem #3:")
  println("avgDelay(flightsMarch3) = " + avgDelay(flightsMarch3))
}

