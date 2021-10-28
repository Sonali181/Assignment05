package com.knoldus.Nat

case class IntNumber(value: Nat, sign: Sign = Positive) extends Nat with Sign{
  def isZero: Boolean = value.isZero

  def predecessor: Nat =
    if (isZero) new IntNumber(value.successor, Negative)
    else if (sign.isPositive) new IntNumber(value.predecessor, sign)
    else new IntNumber(value.successor, Negative)

  def successor: Nat =
    if (isZero) new IntNumber(value.successor, Positive)
    else if (sign.isPositive) new IntNumber(value.successor, sign)
    else new IntNumber(value.predecessor, Negative)

  def +(that: Nat): Nat =
    if (isZero) that
    else if (sign.isPositive) this.predecessor + that.successor
    else this.successor + that.predecessor

  def -(that: Nat): Nat =
    if (that.isZero) this
    else that match {
      case IntNumber(v, s) => this + new IntNumber(v, s.negate)
    }

  def isPositive: Boolean = sign.isPositive

  def negate: IntNumber = new IntNumber(value, sign.negate)

  val toInt: Int = if (sign.isPositive) value.toInt else -value.toInt

  override def toString =
    "" + { if (this.isZero) "" else if (sign.isPositive) "+" else "-" } + value.toInt
}