package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {


  // assignment ::= identifier ~ ":=" ~ expression
  def assignment: Parser[Assignment] =
    identifier ~ ":=" ~ expression ^^ {
      case ident ~ ":=" ~ exp => Assignment(ident, exp)
    }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] =
    "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
      case "while" ~ "(" ~ exp1 ~ ")" ~ exp2 => Iteration(exp1, exp2)
    }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] =
    "[" ~ expression ~ "]" ^^ {
      case "[" ~ exp ~ "]" => FunCall(Identifier("dereference"), List(exp))
    }

  // break ::= "break"
  def break: Parser[Break] =
    "break" ^^ {
      case "break" => Break()
    }

  // ice ::= "<" ~ expression ~ (";" ~ expression)* ~ ">"
  def ice: Parser[Lambda] =
    "<" ~ expression ~ rep(";" ~> expression) ~ ">" ^^ {
      case "<" ~ exp ~ exps ~ ">" => Lambda(Nil, Block(exp::exps))
    }

  def loop: Parser[Loop] =
    "loop" ~ "[" ~ expression ~ "]" ~ expression ^^ {
      case "loop" ~ "[" ~ exp1 ~ "]" ~ exp2 => Loop(exp1, exp2)
  }

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = loop | ice | break | lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"

}