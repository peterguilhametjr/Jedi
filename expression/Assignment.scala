package expression

import context._
import value._

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm:
  override def execute(env: Environment): Value =
    env.apply(vbl) match
      case variable: Variable =>
        variable.content = update.execute(env)
        Notification.DONE
      case _ =>
        throw TypeException(env(vbl).getClass.toString)


    //1. check to see that env(vbl) is a variable, if not throw a type exception
    //2. execute update
    //3. vbl.content = update value
