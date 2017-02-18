package me.rexim.morganey.autocompletion

import me.rexim.morganey.interpreter.ReplContext
import me.rexim.morganey.module._

import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import org.mockito.Mockito._
import org.mockito.Matchers._

class ReplAutocompletionSpec extends FlatSpec with Matchers with MockitoSugar {
  "lastNameInLine" should "extract identifier suffix if it is present" in {
    ReplAutocompletion.lastNameInLine("hello") should be (Some((0, "hello")))
    ReplAutocompletion.lastNameInLine("#$%^ hello") should be (Some(5, "hello"))
    ReplAutocompletion.lastNameInLine("$#*(&* hello5") should be (Some(7, "hello5"))
  }

  it should "return nothing if there is no identifier suffix" in {
    ReplAutocompletion.lastNameInLine("") should be (None)
    ReplAutocompletion.lastNameInLine("hello #$%^") should be (None)
    ReplAutocompletion.lastNameInLine("#$%^ hello &#*$#") should be (None)
  }

  "matches" should "answer true on empty name and any definition" in {
    ReplAutocompletion.matches("", "") should be (true)
    ReplAutocompletion.matches("khooy", "") should be (true)
  }

  it should "answer true if name is a prefix of definition" in {
    ReplAutocompletion.matches("foo", "foo") should be (true)
    ReplAutocompletion.matches("foobar", "foo") should be (true)
  }

  it should "answer false if name is not a prefix of definition" in {
    ReplAutocompletion.matches("", "foo") should be (false)
    ReplAutocompletion.matches("bar", "foo") should be (false)
    ReplAutocompletion.matches("barfoo", "foo") should be (false)
  }

  it should "answer true if name is a prefix of definition case-insensitively" in {
    ReplAutocompletion.matches("FOO", "foo") should be (true)
    ReplAutocompletion.matches("fOobar", "foo") should be (true)
  }

  "matchingDefinitions" should "return nothing on empty line without any known variables" in {
    ReplAutocompletion.matchingDefinitions("", Nil) should be (Nil)
  }

  it should "return all of the known variables on empty line" in {
    val knownVariableNames = List("a", "b")
    ReplAutocompletion.matchingDefinitions("", knownVariableNames) should be (knownVariableNames)
  }

  it should "return autocompletion options on a given prefix" in {
    val knownVariableNames = List("aa", "ab", "ba", "bb")
    ReplAutocompletion.matchingDefinitions("a", knownVariableNames) should be (List("aa", "ab"))
  }

  "autocompleteLoadStatement" should "should autocomplete modules according to the Morganey Index" in {
    val moduleFinderMock = mock[ModuleFinder]

    when(moduleFinderMock.findAllModulesInIndex()).thenReturn(List(
      "foo.mgn",
      "bar.mgn",
      "a/b.mgn"
    ).map(path => new Module(ResourcePath(path))))

    val context = ReplContext(
      bindings = Nil,
      moduleFinder = moduleFinderMock
    )

    ReplAutocompletion.autocompleteLoadStatement(Nil, false, context).sorted should be (List("foo", "bar", "a.b").sorted)
    ReplAutocompletion.autocompleteLoadStatement(List("a"), true, context).sorted should be (List("a.b"))
  }
}
