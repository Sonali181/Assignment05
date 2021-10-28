package com.knoldus.Nat

object Negative extends Sign {
  override def isPositive: Boolean = false
  override def negate: Sign = Positive
}