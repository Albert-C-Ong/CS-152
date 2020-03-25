
package expression

/* -> 3
 * 3
 * -> true
 * true
 * -> "hi"
 * "hi"
 * -> 3.14
 * 3.14
 * 
 */

import value.Value
import expression.Expression
import context.Environment


trait Literal extends Expression with Value {
  def execute(env: Environment) = this
}
