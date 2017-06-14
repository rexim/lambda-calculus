package me.rexim.morganey

import me.rexim.morganey.interpreter.{ReplContext, ReplResult}
import me.rexim.morganey.reduction.Computation
import me.rexim.morganey.syntax._

import scala.util._

import java.util.regex.Pattern

object Commands {

  type CommandTask = ReplContext => Computation[ReplResult[String]]

  sealed trait Command {
    def name: String
    def task: String => ReplContext => ReplResult[String]
  }
  case class StringCommand(name: String, task: String => ReplContext => ReplResult[String]) extends Command
  case class TermCommand(name: String, task: String => ReplContext => ReplResult[String]) extends Command

  private def command(f: ReplContext => ReplResult[String]): CommandTask =
    { context: ReplContext => Computation(f(context)) }

  private def unknownCommand(command: String): CommandTask =
    { context: ReplContext => Computation.failed(new RuntimeException(s"Unknown command '$command'!")) }

  val commandPattern  = ":([a-zA-Z]*)".r
  val commandWithArgs = ":([a-zA-Z]*) (.*)".r
  val unbindCountThreshold = 10

  val commands: Map[String, Command] =
    Seq[Command](
      StringCommand("exit",   exitREPL),
      StringCommand("quit",   exitREPL),
      StringCommand("raw",    rawPrintTerm),
      TermCommand  ("unbind", unbindBindings),
      StringCommand("bindings", printBindings)
    )
    .map(x => x.name -> x)
    .toMap

  def parseCommand(line: String): Option[(String, String)] =
    line match {
      case commandPattern(cmd)        => Some((cmd, ""))
      case commandWithArgs(cmd, args) => Some((cmd, args))
      case _                          => None
    }

  def unapply(line: String): Option[CommandTask] =
    parseCommand(line) map {
      case (cmd, args) if commands contains cmd => command(commands(cmd).task(args.trim))
      case (cmd, _)                             => unknownCommand(cmd)
    }

  private def exitREPL(args: String)(context: ReplContext): ReplResult[String] =
    sys.exit(0)

  private def printBindings(args: String)(context: ReplContext): ReplResult[String] =
    ReplResult(context, Some(context.bindings.map(_.variable.name).mkString(", ")))

  private def rawPrintTerm(args: String)(context: ReplContext): ReplResult[String] = {
    val parseResult = LambdaParser.parseAll(LambdaParser.term, args).toTry
    val output = parseResult match {
      case Success(term) => term.toString
      case Failure(e)    => e.getMessage
    }
    ReplResult(context, Option(output))
  }

  private[morganey] def validRegex(regex: String): Option[String => Boolean] =
    Try {
      val pattern = Pattern.compile(regex)
      s: String => pattern.matcher(s).matches()
    }.toOption

  private def unbindBindings(args: String)(context: ReplContext): ReplResult[String] =
    validRegex(args) match {
      case None =>
        ReplResult(context, Some(s"'$args' is not a valid regular expression!"))
      case Some(matcher) =>
        val (newContext, removedBindings) = context.removeBindings(b => !matcher(b.variable.name))

        val removed = removedBindings.map(_.variable.name).distinct
        val message = removed match {
          case Nil       => "No bindings were removed!"
          case hd :: Nil => s"Binding '$hd' was removed!"
          case bindings if bindings.size > unbindCountThreshold =>
            s"${bindings.size} bindings were removed!"
          case in :+ ls  =>
            val bindingEnumeration = in.map(b => s"'$b'").mkString(", ") + s", and '$ls'"
            s"Bindings $bindingEnumeration were removed!"
        }

        ReplResult(newContext, Option(message))
    }

}
