package expression

import context._
import value._

case class Conditional(val cond: Expression, val cons: Expression, val alter: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    val condition = cond.execute(env)
    if (!condition.isInstanceOf[Boole]) throw TypeException("Conditional condition must be Boole")
    if(condition.asInstanceOf[Boole].value) cons.execute(env)
    else {
      if(alter == null) Notification.UNSPECIFIED
      else alter.execute(env)
    }
  }
}