package com.knoldus
import scala.sys.error


class ObjectAndClass {

  object CallNat extends App {
    val zero = new Succ(Zero)
    val one = new Succ(zero)
    val integer = IntNum(one)
    val integer1 = IntNum(zero, Negative)
    println(integer)
    println(integer1)

  }

  case class IntNum(value: Nat, sign: Sign = Positive) extends Nat with Sign{
    def isZero: Boolean = value.isZero
    def predecessor: Nat =
      if (isZero) new IntNum(value.successor, Negative)
      else if (sign.isPositive) new IntNum(value.predecessor, sign)
      else new IntNum(value.successor, Negative)

    def successor: Nat =
      if (isZero) new IntNum(value.successor, Positive)
      else if (sign.isPositive) new IntNum(value.successor, sign)
      else new IntNum(value.predecessor, Negative)

    def +(that: Nat): Nat =
      if (isZero) that
      else if (sign.isPositive) this.predecessor + that.successor
      else this.successor + that.predecessor

    def -(that: Nat): Nat =
      if (that.isZero) this
      else that match {
        case IntNum(v, s) => this + new IntNum(v, s.negate)
      }

    def isPositive: Boolean = sign.isPositive
    def negate: IntNum = new IntNum(value, sign.negate)
    val toInt: Int = if (sign.isPositive) value.toInt else -value.toInt
    override def toString =
      "" + { if (this.isZero) "" else if (sign.isPositive) "+" else "-" } + value.toInt
  }

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def +(that: Nat): Nat
    def -(that: Nat): Nat
    def toInt: Int
  }

  object Negative extends Sign {
    override def isPositive: Boolean = false
    override def negate: Sign = Positive
  }

  object Positive extends Sign {
    override def isPositive: Boolean = true
    override def negate: Sign = Negative
  }

  trait Sign {
    def isPositive: Boolean
    def negate: Sign
  }

  class Succ(x: Nat) extends Nat {
    def isZero: Boolean = false
    def predecessor: Nat = x
    def successor: Nat = new Succ(this)
    def +(that: Nat): Nat = x + that.successor
    def -(that: Nat): Nat = if (that.isZero) this else x - that.predecessor
    val toInt: Int = x.toInt + 1

  }

  object Zero extends Nat {
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("Zero doesn't have a predecessor")
    def successor: Nat = new Succ(Zero)
    def +(that: Nat): Nat = that
    def -(that: Nat): Nat = if (that.isZero) Zero else error("negative number")

    val toInt: Int = 0
  }

}
