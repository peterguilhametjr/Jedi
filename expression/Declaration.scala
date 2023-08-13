package expression
import context.Environment
import value.Value
import value.Notification

case class Declaration(val ident: Identifier, val body: Expression) extends SpecialForm:
  override def execute(env: Environment): Value =
    env(ident) = body.execute(env)
    Notification.OK

