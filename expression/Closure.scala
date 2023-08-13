package expression

import context._
import value._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value:
  def apply(args: List[Value]): Value =
    //1. create TE = temporary environment extending defEnv
    val tempEnv = new Environment(defEnv)
    //2. add params = args to TE
    tempEnv.bulkPut(params, args)
    //3. execute body in TE
    body.execute(tempEnv)