package me.rexim.morganey

import scala.collection.generic.CanBuildFrom
import scala.util.Try
import scala.language.higherKinds

package object monad {
  /**
    * Returns 'None', if one of the Options in 'lst' is 'None',
    * otherwise the elements are collected in a 'Some'.
    */
  def sequence[CC[+T] <: TraversableOnce[T], T](lst: CC[Option[T]])
                                              (implicit cbf: CanBuildFrom[Nothing, T, CC[T]]): Option[CC[T]] = {
    var out = Option(cbf.apply())
    val i   = lst.toIterator
    while (out.isDefined && i.hasNext)
      i.next() match {
        case Some(elem) => out.map(_ += elem)
        case None       => out = None
      }
    out.map(_.result())
  }

  def sequence[T](lst: List[Try[T]]): Try[List[T]] =
    lst.foldRight(Try(List.empty[T])) {
      case (ele, acc) => acc.flatMap(lst => ele.map(_ :: lst))
    }

  def sequenceRight[L, R](lst: List[Either[L, R]]): Either[L, List[R]] =
    lst.foldRight[Either[L, List[R]]](Right(List.empty[R])) {
      case (ele, acc) => acc.right.flatMap(lst => ele.right.map(_ :: lst))
    }
}
