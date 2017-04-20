package me.rexim.morganey.reduction

import me.rexim.morganey.ast._

object CallByName {
  implicit class CallByNameStrategy(val term: LambdaTerm) {
    def cbnReduce(): LambdaTerm = {
      var result = term
      while (!result.cbnIsFinished()) {
        result = result.cbnStepReduce()
      }
      result
    }

    def cbnStepReduce(): LambdaTerm = term match {
      case LambdaApp(LambdaFunc(x, t), r) => t.substitute(x -> r)
      case LambdaApp(l, r) => LambdaApp(l.cbnStepReduce(), r)
      case input: LambdaInput => input.forceNextChar()
      case other => other
    }

    def cbnIsFinished(): Boolean = term match {
      case LambdaApp(LambdaFunc(_, _), _) => false
      case LambdaApp(l, _) => l.cbnIsFinished()
      case _: LambdaInput => false
      case _ => true
    }
  }

  // TODO: Call-by-name strategy should support collapsed applications
}
