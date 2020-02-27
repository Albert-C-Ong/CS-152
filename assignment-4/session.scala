
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
  
  var inputs = List(1, 2, 3, 4, 5)
  
  println("Problem #1: ")
  println("inputs = " + inputs)
  println("sumCubes1(inputs) = " + sumCubes1(inputs))
  println("sumCubes2(inputs) = " + sumCubes2(inputs))
  println("sumCubes3(inputs) = " + sumCubes3(inputs))
  println("sumCubes4(inputs) = " + sumCubes4(inputs) + "\n")
  
  
  //====================================================================
  // Problem #2
  //====================================================================
  
  // Iterative version
  def sumOfSums1(nums: List[List[Int]]): Int = {
    
    var sum = 0
    
    for (row <- nums) {
      for (num <- row) {
        sum += num
      }
    }
    
    sum
  }
  
  
  // Recursive version
  def sumOfSums2(nums: List[List[Int]]): Int = {
    
    var list_sum = nums(0).foldLeft(0)(_ + _)
    
    if (nums.length == 1) {
      list_sum
    }
    else {
      list_sum + sumOfSums2(nums.drop(1))
    }
  }
  
  
  // Tail-recursive version
  def sumOfSums3(nums: List[List[Int]], sum: Int = 0): Int = {
    
    var total = sum + nums(0).foldLeft(0)(_ + _)
    
    if (nums.length == 1) {
      total
    }
    else {
      sumOfSums3(nums.drop(1), total)
    }
  }
  
    // Pipeline version
  def sumOfSums4(nums: List[List[Int]]): Int = {
    nums.map(x => x.reduce((a, b) => a + b)).reduce((c, d) => c + d)
  }
  
  var nums = List(List(1, 2, 3), List(4, 5, 6))
  
  println("Problem #2: ")
  println("nums = " + nums)
  println("sumOfSums1(nums) = " + sumOfSums1(nums))
  println("sumOfSums2(nums) = " + sumOfSums2(nums))
  println("sumOfSums3(nums) = " + sumOfSums3(nums))
  println("sumOfSums4(nums) = " + sumOfSums4(nums) + "\n")
  
  
  //====================================================================
  // Problem #6
  //====================================================================
  
  
  
  //====================================================================
  // Problem #7
  //====================================================================
  
  
  
  
  //====================================================================
  // Problem #13
  //====================================================================
  
  
  var a13 = Stream.from(1).map(math.pow(_, 0).toInt) // all 1's
  var b13 = Stream.from(1) // all non-negative integers
  var c13 = b13.map(_ * 2) // all non-negative even integers
  var d13 = b13.map(math.pow(_, 2).toInt) // all squares of integers
  
  
  println("Problem #13: ")
  for (stream <- Array(a13, b13, c13, d13)) {
    println(stream + " = " + stream.take(10).toList)
  }
  println()

  
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

