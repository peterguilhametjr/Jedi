package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value =
    val arg = operands.map(_.execute(env))
    if(env.contains(operator)) {
      env(operator) match
        case closure: Closure => closure.apply(arg)
        case _ => throw UndefinedException(operator)
    }
    else{
      alu.execute(operator, arg)
    }
}