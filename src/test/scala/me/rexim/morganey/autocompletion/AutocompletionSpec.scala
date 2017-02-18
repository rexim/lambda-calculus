package me.rexim.morganey.autocompletion

import java.io.File

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.ast.MorganeyBinding
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.interpreter.ReplContext
import me.rexim.morganey.module._
import me.rexim.morganey.Commands
import org.scalatest._
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._

/** Integration tests for Morganey autocompletion mechanism
  */
class AutocompletionSpec extends FlatSpec with Matchers with TestTerms with MockitoSugar {
  private val manyBindings = List("SUCC", "PRED", "MULT", "PLUS")

  private val modules = List(
    "std/boolean.mgn",
    "std/math/arithmetic.mgn",
    "std/list.mgn",
    "std/prelude.mgn"
  ).map(resourcePath => new Module(ResourcePath(resourcePath)))

  private val mockModuleFinder = {
    val moduleFinder = mock[ModuleFinder]
    when(moduleFinder.findAllModulesInIndex()).thenReturn(modules)
    moduleFinder
  }

  private def autocomplete(line: String, cursor: Int,
                           knownNames: List[String],
                           moduleFinder: ModuleFinder = mockModuleFinder): Set[String] = {
    val id = I(lvar("x"))
    val fakeBindings = knownNames.map(name => MorganeyBinding(lvar(name), id))
    val context = ReplContext(fakeBindings, moduleFinder)
    ReplAutocompletion.complete(line, cursor, context).toSet
  }

  "The repl" should "not autocomplete, if no bindings are known" in {
    autocomplete("", 0, List())   should be (Set())
    autocomplete("SU", 2, List()) should be (Set())
    autocomplete("t", 1, List())  should be (Set())
  }

  it should "not autocomplete, if no binding matches the typed text" in {
    autocomplete("t", 1, manyBindings)    should be (Set())
    autocomplete("te", 2, manyBindings)   should be (Set())
    autocomplete("term", 3, manyBindings) should be (Set())
  }

  it should "autocomplete a name, if the typed text matches the name" in {
    val bindings = List("SUCC")
    val expect = bindings.toSet
    autocomplete("s", 1, bindings)   should be (expect)
    autocomplete("sU", 2, bindings)  should be (expect)
    autocomplete("Su", 2, bindings)  should be (expect)
    autocomplete("suc", 3, bindings) should be (expect)
  }

  it should "autocomplete names, if the typed text matches them" in {
    val bindings = List("SUCC1", "SUCC2")
    val expect = bindings.toSet
    autocomplete("s", 1, bindings)   should be (expect)
    autocomplete("sU", 2, bindings)  should be (expect)
    autocomplete("Su", 2, bindings)  should be (expect)
    autocomplete("suc", 3, bindings) should be (expect)
  }

  it should "autocomplete names containing numbers, if the typed text matches them" in {
    val bindings = List("a123", "a987")
    autocomplete("a1", 2, bindings) should be (Set("a123"))
    autocomplete("a9", 2, bindings) should be (Set("a987"))
  }

  it should "autocomplete all known names, if nothing was typed into the repl" in {
    val expect = manyBindings.toSet
    autocomplete("", 0, manyBindings) should be (expect)
  }

  it should "autocomplete all known names in complex structures, too" in {
    autocomplete("(Su PLUS)", 3, manyBindings) should be (Set("(SUCC"))
    autocomplete("(\\x . m)", 7, manyBindings) should be (Set("(\\x . MULT"))
    autocomplete("(\\x . p)", 7, manyBindings) should be (Set("(\\x . PLUS", "(\\x . PRED"))
  }

  it should "autocomplete paths in load statements" in {
    def addLoad(s: String): String = s"load $s"

    autocomplete("load .", 6, List())        should be (Set() map addLoad)
    autocomplete("load ", 5, List())         should be (modules.map(_.name).map(addLoad).toSet)
    autocomplete("load std.m", 10, List())   should be (Set("load std.math.arithmetic"))
  }

  it should "autocomplete commands" in {
    def addColon(s: String): String = ":" + s
    val allCommands = Commands.commands.values map (_.name) map addColon

    autocomplete(":exi", 4, List())  should be (Set("exit") map addColon)
    autocomplete(":unbi", 4, List()) should be (Set("unbind") map addColon)
    autocomplete(":", 1, List())     should be (allCommands.toSet)
  }

  it should "not remove a part of the text, which was typed in, during autocompletion" in {
    val bindings = List("isa", "isb")
    autocomplete("(isa", 4, bindings) should be (Set("(isa"))
    autocomplete("(isb", 4, bindings) should be (Set("(isb"))
    autocomplete("(is", 3, bindings)  should be (Set("(isa", "(isb"))
  }
}
