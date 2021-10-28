package com.knoldus.Nat

object NatCalling extends App {
  val zero = new Succ(Zero)
  val one = new Succ(zero)

  val integer = IntNumber(one)
  val integer1 = IntNumber(zero, Negative)
  println(integer)
  println(integer1)

}