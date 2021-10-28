package com.knoldus.Nat

import scala.sys.error

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Exception("Zero doesn't have a predecessor")

  def successor: Nat = new Succ(Zero)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) Zero else error("negative number")

  //  override def toString = "0 => Zero"

  val toInt: Int = 0
}
