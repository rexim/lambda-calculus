package me.rexim.morganey.module

import org.scalatest._

class ModuleSpec extends FlatSpec with Matchers {
  "Module" should "return its canonical path" in {
    new Module(ResourcePath("a/b/c.mgn")).canonicalPath should be ("a.b.c")
  }
}
