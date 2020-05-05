
package value

import expression._

class Text(val body: Expression) extends Value

object Text {
  def apply(body: Expression) = new Text(body)
}
