package value

import context.*

case class Inexact(value: Double) extends Numeric with Ordered[Value]  {

  def +(other: Value): Addable =
    other match {
      case x: Inexact => Inexact(this.value + x.value)
      case x: Exact => Inexact(this.value + x.value.toDouble)
      case _ => throw new TypeException("Numeric operand required")
    }

  def -(other: Value): Numeric =
    other match {
      case x: Inexact => Inexact(this.value - x.value)
      case x: Exact => Inexact(this.value - x.value.toDouble)
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_- : Numeric = Inexact(-this.value)

  def *(other: Value): Numeric = {
    other match {
      case x: Inexact => Inexact(value * x.value)
      case x: Exact => Inexact(this.value * x.value.toDouble)
      case _ => throw new TypeException("Numeric operand required")
    }
  }

  def /(other: Value): Numeric = {
    other match {
      case x: Inexact => {
        if (x.value == 0) throw new IllegalValueException("Divide by 0!")
        Inexact(value / x.value)
      }
      case x: Exact => {
        if (x.value == 0) throw new IllegalValueException("Divide by 0!")
        Inexact(value / x.value.toDouble)
      }
      case _ => throw new TypeException("Numeric operand required")
    }
  }

  // the following are different because they return Scala values:

  override def compare(other: Value): Int = {
    other match {
      case x: Inexact => value.compare(x.value)
      case x: Exact => value.compare(x.value)
      case _ => throw new TypeException("arguments must be ordered ")
    }
  }

  override def toString: String = value.toString

  override def hashCode = this.toString.hashCode

  override def equals(other: Any) = {
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value
      case x: Exact => x.isInstanceOf[Exact] && x.value.toDouble == this.value
      case _ => false
    }
  }
}