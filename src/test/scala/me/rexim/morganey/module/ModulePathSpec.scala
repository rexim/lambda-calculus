package me.rexim.morganey.module

import org.scalatest._

class ModulePathSpec extends FlatSpec with Matchers {
  private val resourcePath = ResourcePath("a/b/c.mgn")
  private val canonicalPath = CanonicalPath("a.b.c")

  behavior of "Resource path"

  it should "convert to itself without changes" in {
    resourcePath.asResourcePath should be (resourcePath)
  }

  it should "convert to correponding canonical path" in {
    resourcePath.asCanonicalPath should be (canonicalPath)
  }

  behavior of "Canonical path"

  it should "convert to itself without changes" in {
    canonicalPath.asCanonicalPath should be (canonicalPath)
  }

  it should "convert to correponding resource path" in {
    canonicalPath.asResourcePath should be (resourcePath)
  }
}
