package me.rexim.morganey.module

object ModulePath {
  val fileExtension = "mgn"
}

sealed trait ModulePath {
  def asCanonicalPath: CanonicalPath
  def asResourcePath: ResourcePath
}

case class CanonicalPath(path: String) extends ModulePath {
  def asCanonicalPath: CanonicalPath = this
  def asResourcePath: ResourcePath =
    ResourcePath(s"${path.replace('.', '/')}.${ModulePath.fileExtension}")
}

case class ResourcePath(path: String) extends ModulePath {
  def asCanonicalPath: CanonicalPath =
    CanonicalPath(path.replace('/', '.').stripSuffix(s".${ModulePath.fileExtension}"))
  def asResourcePath: ResourcePath = this
}
