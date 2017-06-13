package me.rexim.morganey.meta

import scala.language.higherKinds

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.church.ChurchNumberConverter.encodeNumber
import me.rexim.morganey.church.ChurchPairConverter.{encodeList, encodePair}

trait Liftable[T] extends (T => LambdaTerm) {
  def apply(x: T): LambdaTerm
}

trait DefaultLiftableInstances {

  implicit val liftInt  = Liftable[Int](encodeNumber)
  implicit val liftChar = Liftable[Char](c => encodeNumber(c.toInt))

  implicit def liftString(implicit lift: Liftable[Seq[Char]]) = Liftable[String] { s =>
    lift(s.toSeq)
  }

  implicit def liftPair[A, B](implicit liftA: Liftable[A], liftB: Liftable[B]): Liftable[(A, B)] =
    Liftable[(A, B)] {
      case (a, b) => encodePair( (liftA(a), liftB(b)) )
    }

  implicit def liftColl[CC[X] <: TraversableOnce[X], A](implicit lift: Liftable[A]): Liftable[CC[A]] =
    Liftable[CC[A]] { xs =>
      val ys = xs map lift
      encodeList(ys.toList)
    }

  implicit val liftId = Liftable[LambdaTerm](identity)

}

object Liftable {

  def apply[T](f: T => LambdaTerm): Liftable[T] =
    new Liftable[T] {
      def apply(x: T): LambdaTerm = f(x)
    }

}
