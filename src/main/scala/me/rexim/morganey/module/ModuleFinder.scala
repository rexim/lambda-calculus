package me.rexim.morganey.module

import java.io.File
import java.net.URL

import scala.io.Source

object ModuleFinder {
  val fileExtension = "mgn"
}

class ModuleFinder(classLoader: ClassLoader = ModuleFinder.getClass.getClassLoader) {

  private def modulePathToRelativeURL(modulePath: String): String =
    modulePath.replace('.', '/')

  def findModuleInClasspath(modulePath: String): Option[URL] = {
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
