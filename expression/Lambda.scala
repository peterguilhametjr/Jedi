package expression

import context._
import value._

case class Lambda(val params: List[Identifier], val body: Expression) extends SpecialForm:
  override def execute(env: Environment): Value =
    val closureClass = new Closure(params, body, env)
    closureClass
