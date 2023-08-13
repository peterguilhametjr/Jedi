package expression

import context._
import value._

case class Disjunction(val operand: List[Expression]) extends SpecialForm:
  override def execute(env: Environment): Value =
    var result = false
    for (exp <- operand if !result)
      exp.execute(env) match
        case b: Boole => result = b.value
        case _ => throw TypeException("Disjunction condition must be Boole")
    Boole(result)


