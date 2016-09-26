package me.rexim.morganey

import me.rexim.morganey.ast._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class CollapsedApplicationSpecs extends FlatSpec with Matchers with TestTerms {
  "A collapsed application" should "collapsed with another collapsed application that has the same left term" in {
    val a = LambdaCollapsedApp(x, y, 1)
    LambdaCollapsedApp(x, a) should be (LambdaCollapsedApp(x, y, 2))
  }
}
