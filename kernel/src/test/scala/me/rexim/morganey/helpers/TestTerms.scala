package me.rexim.morganey.helpers

import me.rexim.morganey.ast.{LambdaFunc, LambdaVar, LambdaTerm}
import me.rexim.morganey.ast.LambdaTermHelpers._

trait TestTerms {
  val b = LambdaVar("b")
  val c = LambdaVar("c")
  val d = LambdaVar("d")
  val e = LambdaVar("e")

  val x = LambdaVar("x")
  val y = LambdaVar("y")
  val z = LambdaVar("z")

  def alphaVar(prefix: String, number: Int) =
    LambdaVar(s"$prefix##$number")

  def I(v : LambdaVar) = LambdaFunc(v, v)

  def pair(first: LambdaTerm, second: LambdaTerm, consName: String = "z") =
    lfunc(consName,
      lapp(
        lapp(lvar(consName), first),
        second))

  val random = scala.util.Random

  def redex(body: LambdaTerm) = {
    val id = s"##${random.nextInt(100500)}"
    lapp(lfunc(id, body), lvar(id))
  }

  val zero = lnested(List("f", "x"), lvar("x"))
  val one = lnested(List("f", "x"), lapp(lvar("f"), lvar("x")))
  val two = lnested(List("f", "x"), lapp(lvar("f"), lapp(lvar("f"), lvar("x"))))

  val `[0 .. 2]` = pair(zero, pair(one, pair(two, zero)))

}
