package me.rexim.morganey.interpreter

import scala.util._

import me.rexim.morganey.ast._
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}
import me.rexim.morganey.module._
import me.rexim.morganey.util._

import java.io.Reader

object MorganeyExecutor {

  def interpretNode(node: MorganeyNode, moduleFinder: ModuleFinder): Try[List[MorganeyBinding]] =
    node match {
      case binding: MorganeyBinding => Success(List(binding))
      case MorganeyLoading(modulePath) => loadModule(modulePath, moduleFinder)
    }

  def loadModuleFromReader(reader: Reader, moduleFinder: ModuleFinder): Try[List[MorganeyBinding]] =
    LambdaParser.parseAll(LambdaParser.module, reader) match {
      case LambdaParser.Success(nodes, _) => {
        sequence(nodes.map(interpretNode(_, moduleFinder))).map(_.flatten)
      }
      case LambdaParser.NoSuccess(message, _) => Failure(new LambdaParserException(message))
    }

  def loadModule(modulePath: String, moduleFinder: ModuleFinder): Try[List[MorganeyBinding]] = {
    lazy val moduleNotFound = Failure(new ModuleNotFoundException(s"$modulePath module was not found"))

    moduleFinder
      .findModuleFile(modulePath)
      .map(Success(_))
      .getOrElse(moduleNotFound)
      .flatMap(withReader(_)(loadModuleFromReader(_, moduleFinder)))
  }
}