
import scala.math.sqrt

object lab1 {
  
  def main(args: Array[String]): Unit = {
    // println("Hello, world!")	  
  }

  
  // problem 1
  def perfectSquare(n: Int): Boolean = {
    
    var is_perfect_square = false
    
    var square_root = Math.sqrt(n)
    var rounded = Math.round(square_root)
  
    is_perfect_square = square_root - rounded == 0
    
    //~ for (x <- 0 to 1) {
		
	  //~ var square_root = Math.sqrt(n)
      //~ var rounded = Math.round(square_root)
      
      //~ is_perfect_square = square_root - rounded == 0
    //~ }
    
    is_perfect_square
  }
  
  //~ println(perfectSquare(36))
  //~ println(perfectSquare(37))
  //~ println(perfectSquare(25))
  
  // problem 2
  def aura(n: Int): Int = {
    
    var positive = n > 0
    var by2 = n % 2 == 0
    var by3 = n % 3 == 0
    
    n match {
	  
	  case n if positive && by2 && !by3 => 1
	  case n if positive && by2 && by3 => 2
	  case n if positive && !by2 && by3 => 3
	  case n => 0
    }
  }
  
  for (x <- 0 to 50) {
    println(x + ": " + aura(x));
  } 
}

