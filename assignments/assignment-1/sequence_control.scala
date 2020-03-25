

/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #1: sequence control
 * 
 * Revision: 2020.02.07
 */

object sequence_control extends App {
	
  //====================================================================
  // Problem #1
  //====================================================================
  
  def kingdom(n: Int): Int = {

    if (n > 10) {
	  
	  if (n % 2 == 0) {
	    
	    if (n % 100 == 0) {
		  2 // kindgom 2
		}
		else {
		  1 // kingdom 1
		}
	  }
	  else {
	    4 // kingdom 4
	  }
    }
    else {
	  3 // kingdom 3
    }
  }

  //~ println("kingdom(5) = " + kingdom(5))
  //~ println("kingdom(10) = " + kingdom(10))
  //~ println("kingdom(100) = " + kingdom(100))
  //~ println("kingdom(101) = " + kingdom(101))


  //====================================================================
  // Problem #2
  //====================================================================

  def order(n: Int): Int = {
    
    if (n > 0) {
		
	  var family = if (n % 3 == 0) 1 else 2
	  var _class = if (n == 50) 3 else 4
	  var genus = if (n % 7 == 0) 5 else 6
	  
	  family * _class + genus
	}
	else {
      0  // order 0
    }
  }

  //~ println("order(9) = " + order(9))
  //~ println("order(-10) = " + order(-10))
  //~ println("order(0) = " + order(0))
  //~ println("order(14) = " + order(14))
  //~ println("order(50) = " + order(50))
  
  
  //====================================================================
  // Problem #3
  //====================================================================
  
  def species(n: Int) = {
   
   /* Original code
    * if (0 < n) if (n % 2 == 0) 1 else 2
    * 
    * The original code is incorrect because it only
    * returns 2 if n is positive and odd, but returns
    * a null if n is negative. 
    */ 
   
    // Returns 2 if n is negative
    if (n > 0) if (n % 2 == 0) 1 else 2 else 2
  }
  
  //~ println(species(2))
  //~ println(species(1))
  //~ println(species(-1))
  
  
  //====================================================================
  // Problem #4
  //====================================================================
  
  def tax(income: Int): Double = {
    
    if (income < 0) throw new Exception("invalid income")
    else if (income < 20000) 0
    else if (income < 30000) income * 0.05
    else if (income < 40000) income * 0.11
    else if (income < 60000) income * 0.23
    else if (income < 100000) income * 0.32
    else income * 0.5
  }
  
  // Test cases
  //~ println("Problem #4")
  //~ println("tax(12300) = " + tax(12300))
  //~ println("tax(29000) = " + tax(29000))
  //~ println("tax(125000) = " + tax(125000))
  //~ println("tax(1000000) = " + tax(1000000))
  
  //~ try {
    //~ tax(-1000000)
  //~ } 
  //~ catch {
    //~ case e: Exception => println("tax(-1000000) = " + e)
  //~ }   
  //~ println()
  
  //====================================================================
  // Problem #5
  //====================================================================
  
  def drawRectangle(height: Int, length: Int) {
    
    for (x <- 1 to height) {
	  println("*" * length)
	}
  }
  
  // Test cases
  //~ println("Problem #5")
  //~ println("drawRectangle(3, 4):")
  //~ drawRectangle(3, 4)
  //~ println("drawRectangle(5, 6):")
  //~ drawRectangle(5, 6)
  
  //====================================================================
  // Problem #6
  //====================================================================
  
  def printSums(n: Int, m: Int) {
    
    for (i <- 0 to n - 1) {
	  
	  for (j <- 0 to m - 1) {
	    println(i + " + " + j + " = " + (i + j));
	  }
	}
  }
  
  // Test cases
  //~ println("Problem #6")
  //~ println("printSums(3, 4)")
  //~ printSums(3, 4)
  
  //====================================================================
  // Problem #8
  //====================================================================
  
  def realm1(n: Int) = if (n > 0 && n % 2 == 1) 1 
                         else throw new Exception()
  def realm2(n: Int) = if (n > 0 && n % 2 == 0 && n % 3 != 0) 2 
                         else throw new Exception()
  def realm3(n: Int) = if (n > 0 && n % 6 == 0 || n % 7 == 0) 3 
                         else throw new Exception()
  def realm(n: Int) = {
    
    if (n > 0)
      
      if (n % 2 == 1) 1
      else if (n % 6 == 0 || n % 7 == 0) 3
      else if (n % 2 == 0 && n % 3 != 0) 2
      else 0
      
    else 0
  }
  
  //~ println("Problem #8")
  //~ println("realm(1) = " + realm(1))
  //~ println("realm(2) = " + realm(2))
  //~ println("realm(4) = " + realm(4))
  //~ println("realm(6) = " + realm(6))
  //~ println("realm(14) = " + realm(14))
  //~ println("realm(-1) = " + realm(-1))
  //~ println()
  
  //====================================================================
  // Problem #9
  //====================================================================
  
  //~ def log(x: Double): Option[Double] = if (x <= 0) None else math.log(x)
  
  //~ def sqrt(x: Double): Option[Double] = if (x <= 0) None else math.sqrt(x)
  
  def sqrtLog(x: Double): Option[Double] = {
    
    var x : Option[Double] = Some(math.sqrt(math.log(x)))
    
    None   
  }
}
