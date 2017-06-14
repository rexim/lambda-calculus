package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module._
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.meta._

import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import org.mockito.Mockito._
import org.mockito.Matchers._

import scala.util._

class ReplContextSpecs extends FlatSpec with Matchers with TestTerms with MockitoSugar {
  behavior of "REPL context"

  it should "answer if it contains a specific binding" in {
    val context = ReplContext(List(
      binding(x, x),
      binding(y, y)
    ))

    context.contains(MorganeyBinding(y, x)) should be (true)
    context.contains(MorganeyBinding(z, x)) should be (false)
  }

  it should "allow add bindings to it" in {
    val binding = MorganeyBinding(x, I(x))
    val context = ReplContext().addBinding(binding)
    context.bindings should be (List(binding))
  }

  it should "keep the last unique binding" in {
    val context = ReplContext(List(binding(z, z)))

    context.addBinding(binding(x, x)).bindings should
      be (List(binding(x, x), binding(z, z)))
    context.addBinding(binding(x, x)).addBinding(binding(x, y)).bindings should
      be (List(binding(x, y), binding(z, z)))
    context.addBindings(List(binding(x, x), binding(x, y), binding(x, z))).bindings should
      be (List(binding(x, z), binding(z, z)))
  }

  it should "partition all known bindings" in {
    val zero  = MorganeyBinding(m"zero",  m"0")
    val one   = MorganeyBinding(m"one",   m"1")
    val two   = MorganeyBinding(m"two",   m"2")
    val three = MorganeyBinding(m"three", m"3")

    val knownBindings = List(zero, one, two, three)
    val context = ReplContext(knownBindings)

    val (satisfyCtx, notSatisfy) = context.removeBindings(_.variable.name endsWith "o")
    val ReplContext(satisfy) = satisfyCtx
    satisfy    should be (List(zero, two))
    notSatisfy should be (List(one, three))
  }

  it should "be constructed from a module" in {
    val successModule = mock[Module]
    val failureModule = mock[Module]
    val bindings = List(
      MorganeyBinding(LambdaVar("a"), LambdaVar("a")),
      MorganeyBinding(LambdaVar("a"), LambdaVar("a"))
    )
    val failure = Failure(new IllegalArgumentException("blah"))

    when(successModule.load()).thenReturn(Success(bindings))
    when(failureModule.load()).thenReturn(failure)

    ReplContext.fromModule(successModule) should be (Success(ReplContext(bindings)))
    ReplContext.fromModule(failureModule) should be (failure)
  }
}
