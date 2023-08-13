package expression

import context._
import value._

case class Block(val exps: List[Expression]) extends SpecialForm:
  override def execute(env: Environment): Value =
    //1. create TE = temporary environment extending env
    val tempEnv = new Environment(env)
    var last :Value = null
    //2. execute exps in TE
    for(e <- exps) {
      last = e.execute(tempEnv)
    }
    //3. return value of last one
    last
