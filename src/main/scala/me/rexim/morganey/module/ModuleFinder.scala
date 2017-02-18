package me.rexim.morganey.module

import java.io.File
import java.net.URL

import scala.io.Source

object ModuleFinder {
  val fileExtension = "mgn"
}

class ModuleFinder(classLoader: ClassLoader = ModuleFinder.getClass.getClassLoader) {
  // TODO: Remove ModuleFinder.findModuleInClasspath
  //
  // All of the code that depends on this function should be
  // refactored accordingly. Getting rid of this function essentially
  // eliminates any violations of Module Entity encapsulation.
  //
  // The only code that uses this function is MorganeyCompiler.
  @deprecated("Use {{Module}} entity instead")
  def findModuleInClasspath(modulePath: String): Option[URL] = {
    def modulePathToRelativeURL(modulePath: String): String =
      modulePath.replace('.', '/')

    val resourcePath = s"${modulePathToRelativeURL(modulePath)}.${ModuleFinder.fileExtension}"
    val resourceUrl = classLoader.getResource(resourcePath)
    Option(resourceUrl)
  }

  def findAllModulesInIndex(): List[Module] = {
    import scala.collection.JavaConversions._

    classLoader
      .getResources("morganey-index")
      .toSet
      .flatMap((url: URL) => Source.fromInputStream(url.openStream()).getLines().toSet)
      .toList
      .map(resourcePath => new Module(ResourcePath(resourcePath)))
  }
}
