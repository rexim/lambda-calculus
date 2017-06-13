package me.rexim.morganey.meta

import me.rexim.morganey.ast.LambdaTerm

/**
  * Helper class to match a constant in the quotation at runtime
  */
class UnapplyConst[T](n: T)(implicit morganeyValue: Unliftable[T]) {

  def unapply(term: LambdaTerm): Boolean = term match {
    case morganeyValue(m) => n == m
    case _ => false
  }

}
