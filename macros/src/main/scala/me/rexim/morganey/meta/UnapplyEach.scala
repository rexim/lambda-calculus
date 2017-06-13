package me.rexim.morganey.meta

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.monad._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Helper class to convert each LambdaTerm to T in a collection
  */
class UnapplyEach[T, CC[+X] <: TraversableOnce[X]](unliftT: Unliftable[T])
                                                  (implicit cbf: CanBuildFrom[CC[LambdaTerm], T, CC[T]],
                                                           cbfo: CanBuildFrom[CC[LambdaTerm], Option[T], CC[Option[T]]]) {

  def unapply(terms: CC[LambdaTerm]): Option[CC[T]] = {
    val unliftedElements = (cbfo() ++= (terms map unliftT.unapply _)).result()
    sequence[CC, T](unliftedElements)(cbf)
  }

}
