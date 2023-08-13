package expression

import context._
import value._

case class Conjuction(val operand: List[Expression]) extends SpecialForm:
  override def execute(env: Environment): Value =
    var result = true
    if(operand.size != 0)
      for(exp <- operand if result)
        exp.execute(env) match
          case b: Boole => result = b.value
          case _ => throw TypeException("Conjuction condition must be Boole")
    Boole(result)



