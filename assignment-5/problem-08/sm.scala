
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: Problem #8 - Stack Machine
 * 
 * Revision: 2020.03.09
 */


trait Command {
  var stack: List[Int] = List()
  var input = 0
  def execute()
}

object StackMachine {
  var program: List[Command] = Nil
  var stack: List[Int] = List()
  
  def run() {
    
    for (command <- program) {
      
      println(stack)
      
      command.stack = stack
      command.execute()
      stack = command.stack
    }
    
    stack = List()
    println()
  }
}


// Push command
class Push(val arg: Int) extends Command {
  input = arg
  
  def execute() { 
    
    if (stack.isEmpty) {
      stack = List(input)
    }
    else {
      stack ++= List(input)
    }
  }
}
object Push {
  def apply(arg: Int) = new Push(arg)
}

// Pop command
class Pop extends Command {
  def execute() { stack = stack.dropRight(1) }
}
object Pop {
  def apply() = new Pop()
}


// Top command
class Top extends Command {
  def execute() { println(stack.last) }
}
object Top {
  def apply() = new Top()
}


// Sum command
class Sum extends Command {
  def execute() { 
    
    if (stack.length < 2) {
      throw new Exception("Not enough elements in stack")
    }
    
    var tail = stack(stack.length - 1) + stack(stack.length - 2)
    stack = stack.dropRight(2)
    stack ++= List(tail)
  }
}
object Sum {
  def apply() = new Sum()
}


// Times command
class Times extends Command {
  def execute() { 
    
    if (stack.length < 2) {
      throw new Exception("Not enough elements in stack")
    }
    
    var tail = stack(stack.length - 1) * stack(stack.length - 2)
    stack = stack.dropRight(2)
    stack ++= List(tail)
  }
}
object Times {
  def apply() = new Times()
}


// Test cases
object sm extends App {
  
  println("Test case #1")
  StackMachine.program = List(Push(3), Push(4), Push(5), Sum(), Times(), Top())
  StackMachine.run()                             //> top = 27
    
  println("Test case #2")
  StackMachine.program = List(Push(10), Push(10), Times(), Push(20), Sum(), Top())
  StackMachine.run()                             //> top = 120
  
  println("Test case #3")
  StackMachine.program = List(Push(5), Push(6), Pop(), Push(13), Times(), Top())
  StackMachine.run()                             //> top = 65
}

