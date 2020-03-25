
object mtreview extends App {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // problem 1
  def zip[S, T](list1: List[S], list2: List[T]): List[(S, T)] = {
     if (list1.size != list2.size) throw new Exception("Lists must have same sizes")
     if (list1 == Nil) Nil
     else {
        val pairs = zip(list1.tail, list2.tail)
        (list1.head, list2.head) :: pairs
     }
  }                                               //> zip: [S, T](list1: List[S], list2: List[T])List[(S, T)]
  
  zip(List(1, 2, 3), List("one", "two", "three")) //> res0: List[(Int, String)] = List((1,one), (2,two), (3,three))
  
  zip(List("pi", "e"), List(3.14, 2.71))          //> res1: List[(String, Double)] = List((pi,3.14), (e,2.71))
  
  def unzip[S, T](pairs: List[(S, T)]): (List[S], List[T]) = {
     if (pairs == Nil) (Nil, Nil)
     else {
        val pairsTail =unzip(pairs.tail)
        (pairs.head._1::pairsTail._1, pairs.head._2::pairsTail._2)
     }
  }                                               //> unzip: [S, T](pairs: List[(S, T)])(List[S], List[T])
  
  unzip(zip(List(1, 2, 3), List("one", "two", "three")))
                                                  //> res2: (List[Int], List[String]) = (List(1, 2, 3),List(one, two, three))

  // problem 2
  def pipe[T, S](f: T=>S, g: T=>S): T=>S = {
     def h(t: T): S =
        try {
           f(t)
        } catch {
           case e: Exception => g(t)
        }
     h _
  }                                               //> pipe: [T, S](f: T => S, g: T => S)T => S
  
  val toInteger = pipe((s: String) => s.toInt, (s: String)=>0)
                                                  //> toInteger  : String => Int = <function1>
  
  toInteger("12345")                              //> res3: Int = 12345
  toInteger("123x45")                             //> res4: Int = 0
  
  
  // problem 3
  def isPal(s: String) = s == s.reverse           //> isPal: (s: String)Boolean
  def length(s: String) = s.size                  //> length: (s: String)Int
  def max(n: Int, m: Int) = if(n < m) m else n    //> max: (n: Int, m: Int)Int
  
  // map-reduce solution
  def maxPal1(words: List[String]): Int = {
     val words2 = words.filter(isPal _).map(length _)
     
     if (words2 == Nil) 0
     else words2.reduce(max _)
  }
                                                  //> maxPal1: (words: List[String])Int
     
  maxPal1(List("mom", "rotator", "cowbells", "dad"))
                                                  //> res5: Int = 7
  // tail recursive solution
  def maxPal2(words: List[String]): Int = {
     def helper(result: Int, unseen: List[String]): Int =
        if (unseen == Nil) result
        else helper(if (isPal(unseen.head)) max(length(unseen.head), result) else result, unseen.tail)
     helper(0, words)
  }                                               //> maxPal2: (words: List[String])Int
  
   println(maxPal2(List("mom", "rotator", "cowbells", "dad")))
                                                  //> res6: Int = 7
  
  // Personal tail recursive solution
  def maxPal3(words: List[String], result: Int = 0): Int = {
    
    if (words == Nil) {
      result
    }
    else {
      
      if (isPal(words.head) && length(words.head) > result) {
        maxPal3(words.tail, length(words.head))
      }
      else {
        maxPal3(words.tail, result)
      }
    }
  }
  
  println(maxPal3(List("mom", "rotator", "cowbells", "dad")))
  
  // problem 4
trait Value

trait Expression {
  def execute: Value
}

trait Literal extends Value with Expression {
   def execute = this
}

class Number (val value: Double) extends Literal {
 override def toString = value.toString
 }
object Number {
   def apply(value: Double) = new Number(value)
}

class Boole (val value: Boolean) extends Literal {
 override def toString = value.toString
 }
object Boole {
   def apply(value: Boolean) = new Boole(value)

}

class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
   def execute =  {
     val arg1 = operand1.execute
     val arg2 = operand2.execute
     if (!arg1.isInstanceOf[Number] || !arg2.isInstanceOf[Number]) {
       throw new Exception("sum inputs must be numbers")
     }
     val num1 = arg1.asInstanceOf[Number]
     val num2 = arg2.asInstanceOf[Number]
     new Number(num1.value + num2.value)
   }
}


// and a companion object
object Sum {
   def apply(operand1: Expression, operand2: Expression) = new Sum(operand1, operand2)
}

/*
class And(val operand1: Expression, val operand2: Expression) extends Expression {
   def execute =  {
     val arg1 = operand1.execute
     val arg2 = operand2.execute
     if (!arg1.isInstanceOf[Boole] || !arg2.isInstanceOf[Boole]) {
       throw new Exception("sum inputs must be numbers")
     }
     val num1 = arg1.asInstanceOf[Boole]
     val num2 = arg2.asInstanceOf[Boole]
     new Boole(num1.value && num2.value)
   }
}
*/

// short circuit version

class And(val operand1: Expression, val operand2: Expression) extends Expression {
   def execute =  {
     val arg1 = operand1.execute
     if (!arg1.isInstanceOf[Boole]) throw new Exception("and inputs must be Booles")
     val b = arg1.asInstanceOf[Boole]
     if (!b.value) arg1
     else {
       val arg2 = operand2.execute
     if (!arg2.isInstanceOf[Boole]) throw new Exception("and inputs must be Booles")
     arg2
     }
   }
}


// and a companion object
object And {
   def apply(operand1: Expression, operand2: Expression) = new And(operand1, operand2)
}



// problem 5 lift

  def lift(f: Double=>Double): Number=>Number = {
    def h(n: Number): Number = Number(f(n.value))
  
    h _
  }
  

// problkem 6 Warrior

class Warrior(val name: String) {
     var health: Int = 100
     var strategy: Warrior=>Unit = null
     
     def attack(opponent: Warrior) {
        println(this.name + " is attacking " + opponent.name)
        strategy(opponent)
        println(opponent.name + ".health = " + opponent.health)     
     }  
  }
  
  def makeCompositeStrategy(strategies: List[Warrior=>Unit]) = {
     
        def compStrat(opponent: Warrior) {
           for(s <- strategies) s(opponent)
        }
        
        compStrat _
     
   }  
  
  
   // Problem #7
   case class Entry(val hour: Int, val temp: Double) {
       override def toString = "[" + hour + ":00 temp = " + temp + " degs]"
    }
    
   val readings = List(Entry(6, 25), Entry(10, 28), Entry(12, 32), Entry(16, 30), Entry(18, 26), Entry(22, 19))
   
   def logProcessor(readings: List[Entry]) = {
     readings.filter(_.hour <= 17).map((x: Entry) => Entry(x.hour, ((9.0 * x.temp) / 5.0) + 32))
   }
   
   println(logProcessor(readings))
   
  
  
   // Problem 8: Color
     class Color(val red: Int, val green: Int, val blue: Int) {
        if (red < 0 || green < 0 || blue < 0)
           throw new Exception("Color components pust be positive")
        if (240 < red || 240 < green || 240 < blue)
           throw new Exception("Color components pust be < 240")
           
        override def equals(other: Any) =
           other.isInstanceOf[Color] &&
           other.asInstanceOf[Color].red == this.red &&
           other.asInstanceOf[Color].green == this.green &&
           other.asInstanceOf[Color].blue == this.blue
           
        override def toString = "Color(" + red + ", " + green + ", " + blue + ")"
        
        override def hashCode = this.toString.hashCode
        
     }
     
     object Color {
        def apply(red: Int, green: Int, blue: Int) = new Color(red, green, blue)
        val RED = Color(240, 0, 0)
        val GREEN = Color(0, 240, 0)
        val BLUE = Color(0, 0, 240)
     }
     
     val purple1 = Color(240, 0, 240)
     val purple2 = Color(240, 0, 240)
  
  
     purple1 == purple2 // = true
  
     val colorNames = Map(purple1 -> "Purple", Color.GREEN -> "Green")
     println(colorNames(purple2)) // = "Purple"
      
  }
