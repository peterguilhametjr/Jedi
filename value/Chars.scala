package value

import context.*
import value.{Addable, Exact, Value}

case class Chars(value: String) extends Addable with Ordered[Value]:

  def size(): Exact =
    Exact(value.length)

  def subChars(to: Exact, from: Exact): Chars =
    val startIndex = to.value
    val endIndex = from.value
    Chars(value.substring(startIndex, endIndex))

  def +(other: Value): Addable =
    other match
      case x: Chars => Chars(this.value + x.value)
      case x: Exact => Chars(this.value + x.value.toString)
      case x: Inexact => Chars(this.value + x.value.toString)
      case _ => throw new TypeException("Numeric operand required")

  def compare(other: Value): Int =
    other match
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")

  override def equals(other: Any): Boolean =
    other match
      case x: Chars => this.value == x.value
      case _ => throw new TypeException("Arguments must be comparable")    

  override def toString: String = this.value

