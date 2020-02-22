
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #3
 * 
 * Revision: 2020.02.21
 */


object combinators extends App {

  //====================================================================
  // Problem #1
  //====================================================================

  def combinator[S](
    f: (S) => S, 
    g: (S) => S): S => S = {
    
    (x: S) => {f(g(x))}
  }
  
  //====================================================================
  // Problem #2
  //====================================================================


  def selfIter[T](f: T => T, n: Int) : T => T = {
    
    if (n == 0) {
      f
    }
    else {
      selfIter(combinator(f, f), n - 1)
    }
  }

  def inc(x: Double) = x + 1
  def double(x: Double) = 2 * x

  println("Problem #2")
  println("selfIter(inc, 0)(1) = 2 ^ 0 + 1 = 1 + 1 =" + selfIter(inc, 0)(1))
  println("selfIter(inc, 1)(1) = 2 ^ 1 + 1 + 2 + 1 = " + selfIter(inc, 1)(1))
  println("selfIter(inc, 4)(9) = 2 ^ 4 + 9 = 16 + 9 = " + selfIter(inc, 4)(9))
  println("selfIter(double, 0)(1) = 2 * 1 = " + selfIter(double, 0)(1))
  println("selfIter(double, 1)(1) = (2 ^ 2) * 1 = " + selfIter(double, 1)(1))
  println("selfIter(double, 4)(9) = (2 ^ 2 ^ 2 ^ 2 ^ 2) * 9 = " + selfIter(double, 4)(9) + "\n")


  //====================================================================
  // Problem #3
  //====================================================================

  def countPass[T](array: Array[T]) = {
    
    var count = 0;
    
    for (x <- array) {
      
      x match {
        case b: Boolean => count += 1
        case default => // do nothing
      }
    }
    
    count
  }
  
  println("Problem #3")
  
  var test = Array(true, 2, false)
  println("# of booleans = " + countPass(test) + "\n")


  //====================================================================
  // Problem #4
  //====================================================================

  def recur(baseVal: Int, 
            combiner: (Int, Int) => Int): Int => Int = {
    
    def f(n: Int): Int = {
      
      if (n == 0) {
        baseVal
      }
      else {
        combiner(n, f(n - 1))
      }
    }
    
    f
  }
  
  def factCombiner(x: Int, y: Int) = x * y
  
  println("Problem #4")
  println("factorial(1) = " + recur(1, factCombiner)(1))
  println("factorial(2) = " + recur(1, factCombiner)(2))
  println("factorial(5) = " + recur(1, factCombiner)(5) + "\n")
  

  //====================================================================
  // Problem #5
  //====================================================================

  def deOptionize[T, S](f: T => Option[S]): T => S = {
    
    def f2(x: T): S = { 
      
      if (f(x) == None) {
        throw new Exception("An error has occurred")
      }
      else {
        f(x).get
      }
    }
    
    f2
  }
  
  def parseDigits(digits: String): Option[Int] = 
   if (digits.matches("[0-9]*")) Some(digits.toInt) else None

  println("Problem #5")
  println("deOptionize(parseDigits)('0123') = " + deOptionize(parseDigits)("0123") + "\n")
  // deOptionize(parseDigits)("abc") // Returns an error

  //====================================================================
  // Problem #6
  //====================================================================

  def iterCombinator[T](f: T => T): (T, Int) => T = {
    
    (init: T, n: Int) => { 
      var result = init
      for (i <- 0 to n) result = f(result)
      result
    }
  }
  
  def square(x: Double) = x * x
  
  println("Problem #6")
  println("iterSquare(2, 0) = " + iterCombinator(square)(2, 0))
  println("iterSquare(2, 1) = " + iterCombinator(square)(2, 1))
  println("iterSquare(2, 2) = " + iterCombinator(square)(2, 2) + "\n")
  

  //====================================================================
  // Problem #7
  //====================================================================

  def cube(n: Int) = n * n * n
  
  def unitTest[T, S](f: T => S, cases: Array[(T, S)]): Int = {
    
    var errors = 0;
    
    for ((input, output) <- cases) {
      if (f(input) != output) errors += 1
    }
    
    errors
  }
  
  println("Problem #7")
  println("number of errors = " + 
           unitTest(cube, Array((1, 1), (2, 8), (3, 9), (4, 64), (5, 124))))

}

