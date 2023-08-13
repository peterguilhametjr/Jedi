package value
import expression.Literal

//Jedi Booles encapsulate Scala Booleans
class Boole(val value: Boolean) extends Literal:
  def &&(other: Boole) = Boole(this.value && other.value)

  def ||(other: Boole) = Boole(this.value || other.value)

  def unary_! : Boole = Boole(!this.value)

  override def toString: String = this.value.toString

object Boole:
  val TRUE = new Boole(true)
  val FALSE = new Boole(false)