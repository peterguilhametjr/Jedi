package expression

import context._
import value._

case class Loop(val count: Expression, val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value =
    var result: Value = Notification.UNSPECIFIED
    var exe1 = count.execute(env)
    if (!exe1.isInstanceOf[Exact]) throw new TypeException("parameter must be int.")
    var exe2 = exe1.asInstanceOf[Exact]
    for(i <- 0 until exe2.value){
      result = body.execute(env)
      exe1 = count.execute(env)
      if (!exe1.isInstanceOf[Exact]) throw new TypeException("parameter must be int.")
      exe2 = exe1.asInstanceOf[Exact]
    }
    Notification.DONE


}
