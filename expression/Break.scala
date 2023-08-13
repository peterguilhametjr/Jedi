package expression
import context._
import value._

case class Break() extends SpecialForm {
  override def execute(env: Environment): Value =
    throw new BreakException
    Notification.DONE
}
