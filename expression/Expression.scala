package expression
import value._
import context.Environment

trait Expression:
  def execute(env: Environment): Value
