package me.rexim.morganey.module

class Module(modulePath: ModulePath) {
  def name: String =
    modulePath.asCanonicalPath.path
}
