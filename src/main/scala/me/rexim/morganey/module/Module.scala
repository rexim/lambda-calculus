package me.rexim.morganey.module

class Module(modulePath: ModulePath, classLoader: ClassLoader = Module.getClass.getClassLoader) {
  def canonicalPath: String =
    modulePath.asCanonicalPath.path
}
