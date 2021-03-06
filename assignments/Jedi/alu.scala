package context

import expression._
import value._
import scala.collection.mutable.ArrayBuffer

/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      
      // operators
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args) 
      case "more" => more(args) 
      case "equals" => equals(args) 
      case "unequals" => unequals(args) 
      case "not" => not(args) 
      
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
      
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      // case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
  private def toInt(arg: Value): Option[Integer] =
    if (arg.isInstanceOf[Integer]) Some(arg.asInstanceOf[Integer]) else None
    
  private def toReal(arg: Value): Option[Real] =
    if (arg.isInstanceOf[Real]) Some(arg.asInstanceOf[Real]) 
    else if (arg.isInstanceOf[Integer]) Some(Integer.intToReal(arg.asInstanceOf[Integer]))
    else None
    
  private def toChars(arg: Value): Option[Chars] =
    if (arg.isInstanceOf[Chars]) Some(arg.asInstanceOf[Chars]) else None
  
  // ADDITION OPERATOR
  private def add(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_+_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_+_)
      else {
        val args4 = args.map(toChars).filter(_ != None)
        if (args4.size == args.size) args4.flatten.reduce(_+_)
        else {
          throw new TypeException("Inputs to + must be numbers or texts")
        }
      }
    }
  }
  
  // MULTIPLICATION OPERATOR  
  private def mul(args: List[Value]) = {  
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_*_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_*_)
      else {
        val args4 = args.map(toChars).filter(_ != None)
        if (args4.size == args.size) args4.flatten.reduce(_+_)
        else {
          throw new TypeException("Inputs to * must be numbers or texts")
        }
      }
    }
  }
  
  // SUBTRACTION OPERATOR  
  private def sub(args: List[Value]) = {  
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_-_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_-_)
      else {
        val args4 = args.map(toChars).filter(_ != None)
        if (args4.size == args.size) args4.flatten.reduce(_+_)
        else {
          throw new TypeException("Inputs to - must be numbers or texts")
        }
      }
    }
  }
  
  // DIVISION OPERATOR  
  private def div(args: List[Value]) = {  
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_/_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_/_)
      else {
        val args4 = args.map(toChars).filter(_ != None)
        if (args4.size == args.size) args4.flatten.reduce(_+_)
        else {
          throw new TypeException("Inputs to / must be numbers or texts")
        }
      }
    }
  }
  
  // LESS THAN OPERATOR  
  def less(args: List[Value]): Value = {
      if (args.length  != 2) throw new TypeException("less expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) < args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) < args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) < args4(1))
          else throw new TypeException("Inputs to < must be numbers or texts")
        }
      }
    }
   
   // GREATER THAN OPERATOR  
   def more(args: List[Value]): Value = {
     if (args.length  != 2) throw new TypeException("more expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) > args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) > args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) > args4(1))
          else throw new TypeException("Inputs to > must be numbers or texts")
        }
      }
    }
  
   // EQUALS OPERATOR
   def equals(args: List[Value]): Value = {
     if (args.length  != 2) throw new TypeException("equals expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) == args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) == args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) == args4(1))
          else throw new TypeException("Inputs to == must be numbers or texts")
        }
      }
    }
  
  // NOT EQUALS OPERATOR
  def unequals(args: List[Value]): Value = {
     if (args.length  != 2) throw new TypeException("unequals expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) != args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) != args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) != args4(1))
          else throw new TypeException("Inputs to != must be numbers or texts")
        }
      }
    }
    
    // NOT OPERATOR
    def not(args: List[Value]): Value = {
     if (args.length  != 1) throw new TypeException("not expects one inputs")
      val args2 = args.map(toInt).filter(_ != None)
      Boole(!(args2(0) == Integer(0)))
    }
  
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }
   
   
   //===================================================================
   // VARIABLE OPS
   //===================================================================
   
   // returns the content of args(0)
   private def dereference(args: List[Value]) = {
     if(args(0).isInstanceOf[Variable]) args(0).asInstanceOf[Variable].content
     else throw new TypeException("args must be of type Variable")
   }
   
   // creates a new variable cobtaining args(0)
   private def makeVar(args: List[Value]) = { Variable(args(0)) }
   
   
   //===================================================================
   // STORE OPS
   //===================================================================
   
   // returns a new store containing args
   private def store(args: List[Value]) = {
     var arrBuff = new ArrayBuffer[Value]
     for (e <- args) arrBuff.append(e)
     Store(arrBuff)   
   }
   
   // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
   private def put(args: List[Value]) = {
     if (args.size != 3)
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
     Notification.DONE
   } 
   
    // rem(p: Integer, s: Store) calls s.rem(p)
    private def rem(args: List[Value]) = {
    if (!args(0).isInstanceOf[Integer]) throw new TypeException("First arg must be of type Integer")
      else if (!args(1).isInstanceOf[Store]) throw new TypeException("Second arg must be of type Store")
      else {
        args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer])
        args(1)
     }
   }
   
    // get(p: Integer, s: Store) calls s.get(p)
    private def get(args: List[Value]) = {
     
    }
   
    // map(f: Closure, s: Store) calls s.map(f)
    private def map(args: List[Value]) = {
      if (!args(0).isInstanceOf[Integer]) throw new TypeException("First arg must be of type Integer")
      else if (!args(1).isInstanceOf[Store]) throw new TypeException("Second arg must be of type Store")
      else args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Integer])
    } 
   
    // filter(f: Closure, s: Store) calls s.filter(f)
    private def filter(args: List[Value]) = {
      if (!args(0).isInstanceOf[Closure]) throw new TypeException("First arg must be of type Closure")
      else if (!args(1).isInstanceOf[Store]) throw new TypeException("Second arg must be of type Store")
      else args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])
    } 
   
    // contains(v: Value, s: Store) calls s.contains(v)
    private def contains(args: List[Value]) = {
      if (!args(0).isInstanceOf[Value]) throw new TypeException("First arg must be of type Value")
      else if (!args(1).isInstanceOf[Store]) throw new TypeException("Second arg must be of type Store")
      else args(1).asInstanceOf[Store].contains(args(0).asInstanceOf[Value])
      
    }
   
    // addLast(v: Value, s: Store) calls s.add(v)
    private def addLast(args: List[Value]) = {
      if (!args(0).isInstanceOf[Value]) throw new TypeException("First arg must be of type Value")
      else if (!args(1).isInstanceOf[Store]) throw new TypeException("Second arg must be of type Store")
      else {
        args(1).asInstanceOf[Store].add(args(0).asInstanceOf[Value])
        Notification.DONE
      }
    }
   
    // size(s: Store) calls s.size
    private def size(args: List[Value]) = {
      if (!args(0).isInstanceOf[Store]) throw new TypeException("First arg must be of type Store")
      else args(0).asInstanceOf[Store].size
    }   
}
