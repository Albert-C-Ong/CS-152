
package value


/*
 * -> 3 + def pi = 3.14 // updates env(pi) = 3.14
 * ok
 * 
 */

class Notification(val name: String) extends Value {
  ;
}

object Notification {
  def apply(name: String) = new Notification(name)
  
  // Pre-defined notifications
  val OK = new Notification("OK")
  val DONE = new Notification("DONE")
  val UNSPECIFIED = new Notification("UNSPECIFIED")
}
