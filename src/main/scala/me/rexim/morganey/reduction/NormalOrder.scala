package me.rexim.morganey.reduction

import me.rexim.morganey.ast._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

object NormalOrder {

  implicit class NormalOrderStrategy(val term: LambdaTerm) {

    def norReduce(): LambdaTerm = {
      var result = term
      while (!result.norIsFinished()) {
        result = result.norStepReduce()
      }
      result
    }

    def norReduceComputation(): Computation[LambdaTerm] = new Computation[LambdaTerm] {
      @volatile var cancelled = false

      override def cancel(): Unit = cancelled = true

      override def future: Future[LambdaTerm] = Future {
        var result = term

        while (!cancelled && !result.norIsFinished()) {
          result = result.norStepReduce()
        }

        if (cancelled) {
          throw new ComputationCancelledException
        }

        result
      }
    }

    // TODO: NormalOrder strategy should support collapsed applications

    def norStepReduce(): LambdaTerm = term match {
      case LambdaApp(LambdaFunc(x, t), r) => t.substitute(x -> r)
      case LambdaApp(l, r) if !l.norIsFinished() => LambdaApp(l.norStepReduce(), r)
      case LambdaApp(l, r) if !r.norIsFinished() => LambdaApp(l, r.norStepReduce())
      case LambdaFunc(x, t) => LambdaFunc(x, t.norStepReduce())
      case input: LambdaInput => input.forceNextChar()
      case other => other
    }

    def norIsFinished(): Boolean = term match {
      case LambdaApp(LambdaFunc(_, _), _) => false
      case LambdaApp(l, r) => l.norIsFinished() && r.norIsFinished()
      case LambdaFunc(_, t) => t.norIsFinished()
      case _: LambdaInput => false
      case _ => true
    }
  }
}
