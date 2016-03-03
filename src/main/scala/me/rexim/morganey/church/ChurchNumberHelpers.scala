package me.rexim.morganey.church

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.reduction.NormalOrder._

object ChurchNumberHelpers {
  val zero = lfunc("f", lfunc("x", lvar("x")))

  // SUCC = λn.λf.λx.f (n f x)
  def succ(number: LambdaTerm) = {
    val sourceCode = "(λn.(λf.(λx.(f ((n f) x)))))"
    LambdaParser
      .parse(LambdaParser.term, sourceCode)
      .map(lapp(_, number).norReduce())
      .get
  }

  // PLUS := λm.λn.λf.λx.m f (n f x)
  def plus(x: LambdaTerm, y: LambdaTerm) = {
    val sourceCode = "(λm.(λn.(λf.(λx.((m f) ((n f) x))))))"
    LambdaParser
      .parse(LambdaParser.term, sourceCode)
      .map(f => lapp(lapp(f, x), y).norReduce())
      .get
  }

  // MULT := λm.λn.λf.m (n f)
  def mult(x: LambdaTerm, y: LambdaTerm) = {
    val sourceCode = "(λm.(λn.(λf.(m (n f)))))"
    LambdaParser
      .parse(LambdaParser.term, sourceCode)
      .map(f => lapp(lapp(f, x), y).norReduce())
      .get
  }
}