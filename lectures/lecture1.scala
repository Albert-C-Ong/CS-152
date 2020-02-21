
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Revision: 2020.02.18
 */

object lecture1 extends App {

  def makeIter[T](initVal: T, combiner: (T, Int) => T) = {
    
    def res(n: Int) = {
      var result = initVal
      for (count <- 1 to n) result = combiner(result, count)
      result
    }
  }
  
  def add(x: Int, y: Int) = x + y
  val tri = makeIter[Int](0, add)
  // println(tri(5))
  
  
  
  val fact = makeIter[Int](1, (x: Int, y: Int) => x * y)
  // println(fact(5))
  
  
  
  def blah(s: String, r: Int) = {
    var result = s
  }
  
  val foop = makeIter[String]("Hello", (x: String, y: Int) => x + x)
  println(foop(3, blah))

}

