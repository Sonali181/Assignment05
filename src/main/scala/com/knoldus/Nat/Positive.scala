package com.knoldus.Nat

object Positive extends Sign {
  override def isPositive: Boolean = true
  override def negate: Sign = Negative
}
