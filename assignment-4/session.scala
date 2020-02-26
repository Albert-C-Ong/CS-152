
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #4: List Processing
 * 
 * # 1, 2, 6, 7, 13, 15, 16
 * 
 * Revision: 2020.02.25
 */

object session extends App {
  

  //====================================================================
  // Problem #1
  //====================================================================
  
  // Iterative version
  def sumCubes1(nums: List[Int]): Int = {
    
    var odds = for (i <- nums if i % 2 == 1) yield i
    var sum = 0
    
    for (num <- odds) {
      sum += num * num
    }
    
    sum
  }
  
  
  // Recursive version
  def sumCubes2(nums: List[Int]): Int = {
    
    var odds = for (i <- nums if i % 2 == 1) yield i
    var square = odds(0) * odds(0)
    
    if (odds.length == 1) {
      return square
    }
    else {
      return square + sumCubes2(odds.drop(1))
    }
  }
  
  
  // Tail-recursive version
  def sumCubes3(nums: List[Int], sum: Int = 0): Int = {
    
    var odds = for (i <- nums if i % 2 == 1) yield i
    var square = odds(0) * odds(0)
    
    if (odds.length == 1) {
      return sum + square
    }
    else {
      return sumCubes3(odds.drop(1), sum + square)
    }
  }
  
  // Pipeline version
  def sumCubes4(nums: List[Int]): Int = {
    
    nums.filter(_ % 2 == 1).map(x => x * x).reduce((a, b) => a + b)
  }
  
  var test = List(1, 2, 3, 4, 5)
  
  //~ println(sumCubes1(test))
  //~ println(sumCubes2(test))
  //~ println(sumCubes3(test))
  //~ println(sumCubes4(test))
  
  
  //====================================================================
  // Problem #2
  //====================================================================
  
  
  //~ def sumOfSums1(nums: List[(Int)]): Int {
    
    //~ var sum = 0
    
    //~ for (row <- nums) {
      //~ for (num <- row) {
        //~ sum += num
      //~ }
    //~ }
    
    //~ sum
  //~ }
  
  //~ var nums = List(List(1, 2, 3), List(4, 5, 6))
  
  //~ println(sumOfSums1(nums))
  
  
  //====================================================================
  // Problem #6
  //====================================================================
  
  
  //====================================================================
  // Problem #7
  //====================================================================
  
  
  //====================================================================
  // Problem #13
  //====================================================================
  
  
  //====================================================================
  // Problem #15
  //====================================================================
  
  def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {
    doc.filter(x => !dictionary.contains(x))
  }
  
  var dict = List("mares", "eat", "oats", "and", "cows", "love", "grass")
  
  var input1 = List("hello", "world")
  var input2 = List("I", "love", "cows", "but", "not", "horses")
  var input3 = List("You", "do", "not", "eat", "grass", "on", "Tuesdays")
  
  println("Problem #15: \nDictionary = " + dict + "\n")
  println("input1 = " + input1 + "\nspellCheck(input1) = " + spellCheck(input1, dict) + "\n")
  println("input2 = " + input2 + "\nspellCheck(input2) = " + spellCheck(input2, dict) + "\n")
  println("input3 = " + input3 + "\nspellCheck(input3) = " + spellCheck(input3, dict) + "\n")
  
  
  //====================================================================
  // Problem #16
  //====================================================================
  
  
  def evalMono(mono: (Double, Double), x: Double): Double = {
    mono._1 * math.pow(x, mono._2)
  }
  
  def evalPoly(poly: List[(Double, Double)], x: Double): Double = {
    
    var sum = 0.0
    
    for (mono <- poly) {
      sum += evalMono(mono, x)
    }
    
    sum
  }
  
  println("Problem #16")
  println("if x = 2, x² + 2x + 1 = " 
    + evalPoly(List((1.0, 2.0), (2.0, 1.0), (1.0, 0.0)), 2.0))
  println("if x = 5, x² + 2x + 1 = " 
    + evalPoly(List((1.0, 2.0), (2.0, 1.0), (1.0, 0.0)), 5.0))
  println("if x = 7, 2x³ + 5x² + 19x + 7 = " 
    + evalPoly(List((2.0, 3.0), (5.0, 2.0), (19.0, 1.0), (7.0, 0.0)), 7.0))
}

