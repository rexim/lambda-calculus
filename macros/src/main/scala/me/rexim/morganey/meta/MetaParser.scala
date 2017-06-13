package me.rexim.morganey.meta

import me.rexim.morganey.ast._
import me.rexim.morganey.syntax.LambdaParser

/**
 * Special version of morganey's parser, that allows
 * - two dots ('..') and
 * - a dollar character ('$')
 * as the start of identifiers, as special-markers for holes.
 * Both markers will be used during quotation.
 */
object MetaParser extends LambdaParser {

  override def variable: Parser[LambdaVar] =
    "((\\.\\.)?\\$)?[a-zA-Z][a-zA-Z0-9]*".r ^^ LambdaVar

}
