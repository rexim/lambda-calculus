package me.rexim.morganey.ast

import me.rexim.morganey.ast.error._
import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.church.ChurchPairConverter._
import me.rexim.morganey.util._

import scala.annotation.tailrec

sealed trait LambdaTerm extends MorganeyNode {
  val freeVars: Set[String]

  def substitute(substitution : (LambdaVar, LambdaTerm)): LambdaTerm

  private def wrapBindings(context: Seq[MorganeyBinding]): LambdaTerm =
    context.foldRight(this) {
      case (MorganeyBinding(variable, value), acc) =>
        LambdaApp(LambdaFunc(variable, acc), value)
    }

  private def addBindingsImpl(context: Seq[MorganeyBinding],
                      currentVars: List[String]): Either[BindingError, LambdaTerm] = {
      val bindings = freeVars.toList.map { x =>
        if (!currentVars.contains(x)) {
          context
            .find(_.variable.name == x)
            .toRight(NonExistingBinding(x))
            .right
            .flatMap { case MorganeyBinding(variable, term) =>
              term
                .addBindingsImpl(context, x :: currentVars)
                .right
                .map(MorganeyBinding(variable, _))
            }
        } else {
          Left(BindingLoop((x :: currentVars).reverse))
        }
      }

      sequenceRight(bindings).right.map(this.wrapBindings(_))
  }

  def addBindings(context: Seq[MorganeyBinding]): Either[BindingError, LambdaTerm] =
    addBindingsImpl(context, List())
}

case class LambdaVar(name: String) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (name == v.name) r else this
  }

  override val freeVars: Set[String] = Set(name)

  override def toString: String = name
}

case class LambdaFunc(parameter: LambdaVar, body: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (parameter == v) {
      this
    } else if (r.freeVars.contains(parameter.name)) {
      val commonFreeVars = r.freeVars ++ body.freeVars

      val newParameter =
        LambdaVar(Stream.from(0)
          .map(number => s"${parameter.name}##$number")
          .dropWhile(commonFreeVars.contains)
          .head)

      val newBody = body.substitute(parameter -> newParameter)
      LambdaFunc(newParameter, newBody.substitute(v -> r))
    } else {
      LambdaFunc(parameter, body.substitute(v -> r))
    }
  }

  override val freeVars: Set[String] = body.freeVars - parameter.name

  private[ast] def nestedFunctions(): (List[LambdaVar], LambdaTerm) = {
    @tailrec
    def go(func: LambdaFunc, acc: List[LambdaVar]): (List[LambdaVar], LambdaTerm) =
      func match {
        case LambdaFunc(v, f: LambdaFunc) => go(f, v :: acc)
        case LambdaFunc(v, b)             => ((v :: acc).reverse, b)
      }
    go(this, Nil)
  }

  override def toString: String = {
    val (vars, body) = nestedFunctions()
    val variables = vars.map(t => s"$t.").mkString("")
    s"λ$variables$body"
  }
}

object LambdaCollapsedApp {
  def apply(leftTerm: LambdaTerm, rightTerm: LambdaTerm): LambdaCollapsedApp = {
    rightTerm match {
      case LambdaCollapsedApp(left, right, n) if left == leftTerm => LambdaCollapsedApp(left, right, n + 1)
      case _ => LambdaCollapsedApp(leftTerm, rightTerm, 1)
    }
  }
}

case class LambdaCollapsedApp(leftTerm: LambdaTerm, rightTerm: LambdaTerm, n: Int) extends LambdaTerm {
  override val freeVars: Set[String] = leftTerm.freeVars ++ rightTerm.freeVars

  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    LambdaApp(
      leftTerm.substitute(v -> r),
      rightTerm.substitute(v -> r))
  }
}

case class LambdaApp(leftTerm: LambdaTerm, rightTerm: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    LambdaApp(
      leftTerm.substitute(v -> r),
      rightTerm.substitute(v -> r))
  }

  override val freeVars: Set[String] = leftTerm.freeVars ++ rightTerm.freeVars

  private[ast] def nestedApplications(): (LambdaTerm, List[LambdaTerm]) = {
    @tailrec
    def go(app: LambdaApp, acc: List[LambdaTerm]): (LambdaTerm, List[LambdaTerm]) =
      app match {
        case LambdaApp(l: LambdaApp, r) => go(l, r :: acc)
        case LambdaApp(l, r)            => (l, r :: acc)
      }
    go(this, Nil)
  }

  override def toString: String = {
    val (deep, nest) = nestedApplications()
    val nested = nest.map {
      case a: LambdaApp  => s"($a)"
      case f: LambdaFunc => s"($f)"
      case t             => t.toString
    }.mkString(" ")
    val rest = deep match {
      case f: LambdaFunc => s"($f)"
      case t             => t.toString
    }
    s"$rest $nested"
  }
}

case class LambdaInput(input: Stream[Char]) extends LambdaTerm {
  override val freeVars: Set[String] = Set()

  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm =
    forceNextChar().substitute(substitution)

  /**
    * Forces next character in the input to be Church encoded and
    * put at the begining of the virtual input list
    */
  def forceNextChar(): LambdaTerm =
    input match {
      case x #:: xs => encodePair((encodeNumber(x.toInt), LambdaInput(xs)))
      case _ => encodeList(List())
    }

  override def toString: String = "<input>"
}
