package me.rexim.morganey.syntax

import me.rexim.morganey.ast._
import me.rexim.morganey.church.{ChurchNumberConverter, ChurchPairConverter}
import me.rexim.morganey.util._
import me.rexim.morganey.syntax.Language._

import scala.collection.immutable.NumericRange
import scala.util.parsing.combinator._
import scala.language.postfixOps

object IntMatcher {
  def unapply(rawInt: String): Option[Int] =
    try {
      Some(Integer.parseInt(rawInt))
    } catch {
      case e: NumberFormatException => None
    }
}

object LambdaParser extends LambdaParser

class LambdaParser extends JavaTokenParsers with ImplicitConversions {

  protected override val whiteSpace = whiteSpacePattern.r

  /* =================================== Literals =================================== */

  def literal: Parser[LambdaTerm] =
    numericLiteral | characterLiteral | stringLiteralTerm | listLiteral

  def validNumberLiteral: Parser[Int] =
    numberLiteral.r ^? ({
      case IntMatcher(x) => x
    }, { (rawInt) => s"`$rawInt' is too big"})

  def numericLiteral: Parser[LambdaTerm] =
    validNumberLiteral ^^ ChurchNumberConverter.encodeNumber

  def validCharacterLiteral: Parser[Char] = (
    escapedCharLiteral.r ^^ { s =>
      escapeSequences(s charAt 2)
    }
    | symbolCharLiteral.r ^^ { _ charAt 1 }
  )

  def characterLiteral: Parser[LambdaTerm] =
    validCharacterLiteral ^^ { c => ChurchNumberConverter.encodeNumber(c.toInt) }

  def stringLiteralTerm: Parser[LambdaTerm] =
    stringLiteral ^^ { s =>
      ChurchPairConverter.encodeString(unquoteString(s))
    }

  private def rangeConstant: Parser[LambdaTerm] = {
    val number = validNumberLiteral | validCharacterLiteral ^^ (_.toInt)
    val parser = number ~ opt(comma ~> number) ~ (rangeOperator ~> number)
    parser ^^ { case start ~ next ~ exit =>
      val step  = next.map(_ - start).getOrElse(1)
      val range = NumericRange.inclusive(start, exit, step).toList
      val nums  = range map ChurchNumberConverter.encodeNumber
      ChurchPairConverter.encodeList(nums)
    }
  }

  def listLiteral: Parser[LambdaTerm] = (
      brackets(repsep(term, comma)) ^^ ChurchPairConverter.encodeList
    | brackets(rangeConstant)
  )

  /* ============================== Core lambda terms =============================== */

  def variable: Parser[LambdaVar] =
    identifier.r ^^ { LambdaVar }

  def func: Parser[LambdaFunc] =
    lambda ~> rep1(variable <~ abstractionDot) ~ term ^^ {
      case (init :+ last) ~ body => init.foldRight(LambdaFunc(last, body))(LambdaFunc)
    }

  // TODO: parser support for collapsed applications

  def application: Parser[LambdaApp] =
    termWithoutApp ~ termWithoutApp ~ rep(termWithoutApp) ^^ {
      case fst ~ mid ~ rest => rest.foldLeft(LambdaApp(fst, mid))(LambdaApp)
    }

  def term: Parser[LambdaTerm] =
    func | application | termWithoutApp

  def termWithoutApp: Parser[LambdaTerm] =
    func | variable | literal | parenthesis(term)

  /* =========================== Morganey extension terms =========================== */

  def binding: Parser[MorganeyBinding] =
    (variable <~ bindingAssign) ~ term ^^ { MorganeyBinding }

  def loading: Parser[MorganeyLoading] =
    loadKeyword ~> (modulePath.r ?) ^^ { MorganeyLoading }

  def replCommand: Parser[MorganeyNode] =
    loading | binding | term

  def script: Parser[List[MorganeyNode]] =
    withLnBreaks(repsep(replCommand, newLines))

  def module: Parser[List[MorganeyNode]] =
    withLnBreaks(repsep(loading | binding, newLines))

  /* ============================== Helper productions ============================== */

  private def lambda = lambdaLetter | lambdaSlash

  private def parenthesis[T](p: Parser[T]): Parser[T] =
    leftParenthesis ~> p <~ rightParenthesis

  private def brackets[T](p: Parser[T]): Parser[T] =
    leftBracket ~> p <~ rightBracket

  private def withLnBreaks[T](p: Parser[T]): Parser[T] =
    optNewLines ~> p <~ optNewLines

  private def newLine: Parser[String] =
    lineBreak.r

  private def optNewLines: Parser[List[String]] =
    rep(newLine)

  private def newLines: Parser[List[String]] =
    rep1(newLine)

}
