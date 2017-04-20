package me.rexim.morganey.syntax

import me.rexim.morganey.ast._
import me.rexim.morganey.church.{ChurchNumberConverter, ChurchPairConverter}
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

  private def rangeParser[T](q: => Parser[T]): Parser[T ~ Option[T] ~ T] = {
    val p = q
    p ~ opt(comma ~> p) ~ (rangeOperator ~> p)
  }

  private def rangeConstant: Parser[LambdaTerm] = {
    val number = validNumberLiteral | validCharacterLiteral ^^ (_.toInt)
    rangeParser(number) ^^ { case start ~ next ~ exit =>
      val step  = next.map(_ - start).getOrElse(1)
      val range = NumericRange.inclusive(start, exit, step).toList
      val nums  = range map ChurchNumberConverter.encodeNumber
      ChurchPairConverter.encodeList(nums)
    }
  }

  def listLiteral: Parser[LambdaTerm] = (
      brackets(repsep(term, comma)) ^^ ChurchPairConverter.encodeList
    | brackets(rangeConstant)
    | brackets {
        rangeParser(term) ^^ { case start ~ next ~ exit =>
          val range         = LambdaVar("range")
          val rangeWithNext = LambdaVar("rangeWithNext")
          next match {
            case None      => LambdaApp(LambdaApp(range, start), exit)
            case Some(nxt) => LambdaApp(LambdaApp(LambdaApp(rangeWithNext, start), nxt), exit)
          }
        }
      }
  )

  private def pair: Parser[LambdaTerm] =
    parenthesis((term <~ comma) ~ term) ^^ {
      case first ~ second => ChurchPairConverter.encodePair((first, second))
    }

  /* ============================== Core lambda terms =============================== */

  def variable: Parser[LambdaVar] =
    not(keywordParser) ~> identifier.r ^^ { LambdaVar }

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
    func | variable | literal | pair | parenthesis(term)

  /* =========================== Morganey extension terms =========================== */

  def binding: Parser[MorganeyBinding] =
    defKeyword ~> (variable <~ bindingAssign) ~ term ^^ { MorganeyBinding }

  def loading: Parser[MorganeyLoading] =
    loadKeyword ~> (modulePath.r ?) ^^ { MorganeyLoading }

  def replCommand: Parser[MorganeyNode] =
    loading | binding | term

  def script: Parser[List[MorganeyNode]] =
    rep(replCommand)

  def module: Parser[List[MorganeyNode]] =
    rep(loading | binding)

  /* ============================== Helper productions ============================== */

  private def lambda = lambdaLetter | lambdaSlash

  private def parenthesis[T](p: Parser[T]): Parser[T] =
    leftParenthesis ~> p <~ rightParenthesis

  private def brackets[T](p: Parser[T]): Parser[T] =
    leftBracket ~> p <~ rightBracket

  private def keywordParser: Parser[String] =
    /* Allow keywords as prefix of identifiers,
     * but disallow keywords used as identifier.
     * see: http://stackoverflow.com/a/3770843
     */
    keywords.map(k => (k + "\\b").r).map(regex).reduceLeft(_ | _)

}
