
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: Problem #7 - Accumulator
 * 
 * Revision: 2020.03.09
 */

import scala.collection._


object accumulator extends App {

  trait Instruction {
    
    var register = 0
    var halt = false
    var ip = 0
    
    def execute()
  }


  object Accumulator {
    
    var register = 0
    var program: List[Instruction] = Nil
    
    var halt = false   // halt flag
    var ip = 0         // instruction pointer
    
    def run() {

      while (ip < program.length) {
        
        program(ip).register = register
        program(ip).ip = ip
        
        program(ip).execute()
        
        halt = program(ip).halt
        
        if (program(ip).ip != ip) {
          ip = program(ip).ip - 1
          program(ip).register = register
        }
        
        if (!halt) {
          register = program(ip).register
          ip += 1
        }
        else {
          ip = program.length
        }
      }
      
      ip = 0 // reset ip
    }
  }


  // Add instruction
  class Add(x: Int) extends Instruction {
    def execute() { register += x}
  }
  object Add {
    def apply(x: Int) = new Add(x); 
  }


  // Multiple instruction
  class Mul(x: Int) extends Instruction {
    def execute() { register *= x}
  }
  object Mul {
    def apply(x: Int) = new Mul(x); 
  }


  // Halt Instruction
  class Halt() extends Instruction {
    def execute() { halt = true }
  }
  object Halt {
    def apply() = new Halt()
  }
  
  // Goto Instruction
  class Goto(arg: Int) extends Instruction {
    def execute() { ip = arg }
  }
  object Goto {
    def apply(arg: Int) = new Goto(arg)
  }

  
  // Test cases
  // computing ((3 * 5) + 1) * 2
  Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
  Accumulator.run()
  println(Accumulator.register)                           //> res6: Int = 32
  
  // computing (((10 * 2) + 3) * 5)
  Accumulator.register = 0
  Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
  Accumulator.run()
  println(Accumulator.register)                           //> res7: Int = 115

  // Halt test case
  // computing (10 * 2)
  Accumulator.register = 0
  Accumulator.program = List(Add(10), Mul(2), Halt(), Add(3), Mul(5))
  Accumulator.run()
  println(Accumulator.register)                           //> res: Int = 20
  
  // Goto test case
  // computing ((8 + 20) * 6)
  Accumulator.register = 0
  Accumulator.program = List(Add(8), Goto(3), Halt(), Add(20), Mul(6))
  Accumulator.run()
  println(Accumulator.register)                           //> res: Int = 168
  


}
