package expression

import context._
import value._

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm:
  override def execute(env: Environment) = {
    var result: Value = Notification.UNSPECIFIED
    var exe1 = condition.execute(env)
    if (!exe1.isInstanceOf[Boole]) throw new TypeException("parameter must be boolean.")
    var exe2 = exe1.asInstanceOf[Boole]
    var break = false
    while (exe2.value && !break) {
      try {
        result = body.execute(env)
        exe1 = condition.execute(env)
        if (!exe1.isInstanceOf[Boole]) throw new TypeException("parameter must be boolean.")
        exe2 = exe1.asInstanceOf[Boole]
      }
      catch {
        case e => break = true
      }
    }
    Notification.DONE
  }

