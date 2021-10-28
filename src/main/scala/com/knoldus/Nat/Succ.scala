package com.knoldus.Nat

class Succ(x: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = x
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat = x + that.successor
  def -(that: Nat): Nat = if (that.isZero) this else x - that.predecessor
  //  override def toString = {
  //    def findNumber(nat: Nat): Int =
  //      if (nat.isZero) 0
  //      else 1 + findNumber(nat.predecessor)
  //    val number = findNumber(this)
  //    String.valueOf(number)
  //  }
  val toInt: Int = x.toInt + 1

}