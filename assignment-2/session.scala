
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #2: Recursion
 * 
 * Revision: 2020.02.14
 */

object session extends App {
  
  def inc(n: Int) = n + 1
  def dec(n: Int) = n - 1
  def isZero(n: Int) = n == 0

  //====================================================================
  // Problem #1
  //====================================================================
  
  def add(n: Int, m: Int): Int = {
    
    if (isZero(m)) {
      n;
    }
    else {
      add(inc(n), dec(m));
    }
  }
  
  // Test cases
  println("Problem #1: ")
  println("add(2, 2) = " + add(2, 2));
  println("add(5, 8) = " + add(5, 8));
  println("add(100, 1) = " + add(100, 1) + "\n");
  
  //====================================================================
  // Problem #2
  //====================================================================
  
  def mul(n: Int, m: Int, product: Int = 0): Int = {
    
    if (isZero(m)) {
      product
    }
    else {
      mul(n, dec(m), add(n, product))
    }
  }
  
  // Test cases
  println("Problem #2: ")
  println("mul(2, 2) = " + mul(2, 2));
  println("mul(5, 8) = " + mul(5, 8));
  println("mul(100, 1) = " + mul(100, 1) + "\n");
  
  //====================================================================
  // Problem #3
  //====================================================================
  
  def exp(m: Int, product: Int = 1): Int = {
    
    if (isZero(m)) {
      product
    }
    else {
      exp(dec(m), mul(2, product))
    }
  }
  
  // Test cases
  println("Problem #3: ")
  println("exp(2) = " + exp(2));
  println("exp(5) = " + exp(5));
  println("exp(10) = " + exp(10) + "\n");
  
  //====================================================================
  // Problem #4
  //====================================================================
  
  
  def hyperExp(n: Int, product: Int = 0): Int = {
    
    if (isZero(n)) {
      product
    }
    else {
      hyperExp(dec(n), exp(product))
    }
  }
  
  println("Problem #4: ")
  for (x <- 1 to 5) {
    println("hyperExp(" + x + ") = " + hyperExp(x))
  }
  println()
  
  //====================================================================
  // Problem #5
  //====================================================================
  
  /* Problems #1-4 are implemented using tail recursion
   * 
   * Tail recusion does not appear to reduce the occurence of stack 
   * overflows. However, computation should improve when using tail
   * recursion due to the reduced number of calculations needed. 
   */ 
  
  //====================================================================
  // Problem #9
  //====================================================================
  
  // Regular recursion
  def fib(n: Int): Int = {
    
    if (n == 0 || n == 1) {
      n
    }
    else {
      fib(n - 2) + fib(n - 1)
    }
  }
  
  // Tail recursion
  def fib2(n: Int, a: Int = 0, b: Int = 1): Int = {
    
    if (n == 0) {
      a
    }
    else if (n == 1) {
      b
    }
    else {
      fib2(n - 1, b, a + b);
    }
  }
  
  // Test cases
  println("Problem #9: \nRegular recusion")
    for (x <- 0 to 10) {
    println("fib(" + x + ") = " + fib(x));
  }
  
  println("\nTail recusion")
  for (x <- 0 to 10) {
    println("fib2(" + x + ") = " + fib2(x));
  }
  println()
  
  //====================================================================
  // Problem #10
  //====================================================================

  def choose(n: Int, m: Int): Int = {
    
    if (n == m || m == 0) {
      1
    }
    else {
      choose(n - 1, m - 1) + choose(n - 1, m)
    }
  }
  
  // Test cases
  println("Problem #10: ")
  
  for (x <- 1 to 5) {
    for (y <- 1 to x) {
      println("choose(" + x + ", " + y + ") = " + choose(x, y));
    }
  }
}

