
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: problem #7
 * 
 * Revision: 2020.03.05
 */

import scala.collection._


object accumulator extends App {

  trait Instruction {
    def execute()
  }
  
  object Accumulator {
    
    var register = 0
    var program: List[Instruction] = Nil
    
    var halt = false   // halt flag
    var ip = 0         // instruction pointer
    
    def run() {
      
      while (ip < program.length) {
        
        program(ip).execute()
        
        ip += 1
      }
      
      // for (inst <- program if !halt) inst.execute()
    }
  }
  
  
  class Add(x: Int) extends Instruction {
    
    def execute() {
      ;
    }
  }
  
  
  // computing ((3 * 5) + 1) * 2
  Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
  Accumulator.run()
  Accumulator.register                           //> res6: Int = 32
  
  // computing (((10 * 2) + 3) * 5)
  Accumulator.register = 0
  Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
  Accumulator.run()
  Accumulator.register                           //> res7: Int = 115
}
