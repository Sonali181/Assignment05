package com.knoldus.Nat

trait Sign {
  def isPositive: Boolean
  def negate: Sign
}