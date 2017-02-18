package me.rexim.morganey.module

import me.rexim.morganey.ast._

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import java.io._
import java.util.Vector
import java.net.URL
import java.nio.charset.StandardCharsets

import scala.util._

class ModuleSpec extends FlatSpec with Matchers with MockitoSugar {
  // TODO: Extract mockClassLoaderResource into reusable thing
  //
  // Reuse that thing in ModuleFinderSpec

  def mockClassLoaderResource(resourcePath: String, resourceContent: String): ClassLoader = {
    val resourceUrl: URL = {
      val mockResourceUrl = mock[URL]
      when(mockResourceUrl.openStream())
        .thenReturn(new ByteArrayInputStream(resourceContent.getBytes(StandardCharsets.UTF_8)))
      mockResourceUrl
    }

    val classLoader: ClassLoader = {
      val mockClassLoader = mock[ClassLoader]
      when(mockClassLoader.getResource(resourcePath)).thenReturn(resourceUrl)
      mockClassLoader
    }

    classLoader
  }

  behavior of "Module"

  it should "return its canonical path" in {
    new Module(ResourcePath("a/b/c.mgn")).canonicalPath should be ("a.b.c")
  }

  it should "parse and return bindings inside of the resource it encapsulates" ignore {
    val moduleContent = """|def a := a
                           |def b := b""".stripMargin
    val resourcePath = "a/b/c.mgn"

    val classLoader: ClassLoader = mockClassLoaderResource(resourcePath, moduleContent)

    new Module(ResourcePath(resourcePath), classLoader).bindings should
      be (Success(Set(
        MorganeyBinding(LambdaVar("a"), LambdaVar("a")),
        MorganeyBinding(LambdaVar("b"), LambdaVar("b"))
      )))
  }

  it should "parse and return module dependencies" ignore {
    val moduleContent = """|load foo
                           |load bar.baz
                           |def c := c""".stripMargin
    val resourcePath = "a/b/c.mgn"

    val classLoader = mockClassLoaderResource(resourcePath, moduleContent)

    new Module(ResourcePath(resourcePath), classLoader).dependencies.map(_.map(_.canonicalPath)) should
      be (Success(Set("foo", "bar.baz")))
  }
}
