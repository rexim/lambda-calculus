package me.rexim.morganey.autocompletion

import java.io.File

import me.rexim.morganey.Commands._
import me.rexim.morganey.ast._
import me.rexim.morganey.interpreter.ReplContext
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.syntax.Language.identifier
import me.rexim.morganey.util._

import extractors._

object ReplAutocompletion {

  // TODO(#257): use classpath modules in ReplAutocompletion

  def complete(buffer: String, context: ReplContext): List[String] =
    complete(buffer, buffer.length, context)

  def complete(buffer: String, cursor: Int, context: ReplContext): List[String] = {
    val knownVariableNames = context.bindings.map(_.variable.name)
    val (beforeCursor, _) = buffer splitAt cursor
    lazy val definitions = matchingDefinitions(beforeCursor, knownVariableNames, cursor)

    beforeCursor match {
      // if a command was (fully) typed in and requires a term as an argument
      case CommandWithArg(cmd, arg)          =>
        knownVariableNames filter (_ startsWith arg) map (b => s":$cmd $b")
      // if a command was typed in
      case SimpleCommand(prefix)             =>
        commands.keys.toList
          .filter(_ startsWith prefix)
          .map(name => s":$name")
      // if a load statement was typed in (potential partially)
      case LoadStatement(parts, endsWithDot) =>
        val autocompletedModules = autocompleteLoadStatement(parts, endsWithDot, context)
        autocompletedModules map (m => s"load $m")
      // if there are no names, REPL can't autocomplete
      case _ if knownVariableNames.isEmpty   => Nil
      // if there are definitions matching the users input, use them for completion
      case _ if definitions.nonEmpty         => definitions
      // if nothing was typed into the repl, autocomplete with all known names
      case _ if buffer.trim.isEmpty          => knownVariableNames
      // if something was typed into the repl, autocomplete with all names starting with the input text
      case _                                 =>
        knownVariableNames filter (matches(_, beforeCursor))
    }
  }

  private def autocompleteLoadStatement(parts: List[String], endsWithDot: Boolean, context: ReplContext): List[String] = {
    import ModuleFinder._

    val moduleFinder = context.moduleFinder


    def stripExtensionIfModuleFile(f: File): File =
      if (isMorganeyModule(f)) new File(f.getParent, f.getName.replaceAll(s".$fileExtension", ""))
      else f

    def everythingIn(path: List[String], fileNameFilter: String => Boolean = _ => true) =
      moduleFinder.findAllModulesIn(path.mkString("."))
        .map { case (root, f) => (root, stripExtensionIfModuleFile(f)) }
        .filter { case (root, f) => fileNameFilter(f.getName) }
        .map(relativize)
        .map(_.replace('/', File.separatorChar))
        .map(relativeFileToLoadPath)

    def relativize(baseAndFile: (File, File)): String = {
      val (base, file) = baseAndFile
      base.toURI().relativize(file.toURI()).getPath()
    }

    def moduleName(file: File): String = {
      val rawName = stripExtensionIfModuleFile(file).getName
      val suffix = if (file.isDirectory) "." else ""
      s"$rawName$suffix"
    }

    (parts, endsWithDot) match {
      // load .|
      case (Nil, true)       => Nil
      // load a.b.|
      case (xs, true)        => everythingIn(xs)
      // load |
      case (Nil, false)      => moduleFinder.topLevelDefinitions().map(moduleName)
      // load math.ari|
      case (xs :+ x, false)  => everythingIn(xs, matches(_, x))
    }
  }

  private object CommandWithArg {

    def unapply(line: String): Option[(String, String)] = {
      val potentialCommand = parseCommand(line)
      potentialCommand flatMap { case (p, arg) =>
        commands.values find {
          case StringCommand(_, _)  => false
          case TermCommand(name, _) => name == p
        } map (_.name -> arg)
      }
    }

  }

  private def matches(definition: String, name: String) =
    definition.toLowerCase startsWith name.toLowerCase

  private def matchingDefinitions(line: String, knownVariableNames: List[String], cursor: Int): List[String] = {
    lazy val globalPrefix = line take cursor
    lazy val allNames     = knownVariableNames map (globalPrefix + _)
    val lastName = lastNameInLine(line)

    val filtered = lastName map { case (off, lname) =>
      val names  = knownVariableNames filter (matches(_, lname))
      val prefix = line take off
      names map (prefix + _)
    }

    // if no names match, autocomplete with all known names
    // e.g.: (|
    filtered.getOrElse(allNames)
  }

  private def lastNameInLine(line: String): Option[(Int, String)] = {
    def stringMatches(n: Int): Option[(Int, String)] =
      Option(line takeRight n)
        .filter(_.matches(identifier))
        .map(id => (line.length - n) -> id)

    def size(str: Option[(Int, String)]): Int =
      str.map { case (_, s) => s.length }.getOrElse(Int.MinValue)

    val lengths = (0 to line.length).toStream
    lengths.map(stringMatches).maxBy(size)
  }

}