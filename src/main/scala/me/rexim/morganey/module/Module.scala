package me.rexim.morganey.module

import me.rexim.morganey.ast.MorganeyBinding
import scala.util._

class Module(modulePath: ModulePath, classLoader: ClassLoader = Module.getClass.getClassLoader) {
  def canonicalPath: String =
    modulePath.asCanonicalPath.path

  // TODO: Implement Module.bindings
  def bindings: Try[Set[MorganeyBinding]] = ???

  // TODO: Implement Module.dependencies
  def dependencies: Try[Set[Module]] = ???
}
