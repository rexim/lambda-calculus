package me.rexim.morganey.meta

import org.scalatest.{FlatSpec, Matchers}

class HoleSpec extends FlatSpec with Matchers {

  behavior of "A hole"

  it should "mark positions, where lambda-terms were inserted during interpolation" in {
    Hole(0)   should be ("$hole0")
    Hole(1)   should be ("$hole1")
    Hole(2)   should be ("$hole2")
    Hole(999) should be ("$hole999")
  }

  it can "be used to get the number back, which was used to create the hole" in {
    val Hole(0)   = "$hole0"
    val Hole(1)   = "$hole1"
    val Hole(2)   = "$hole2"
    val Hole(999) = "$hole999"
  }

  it should "throw a MatchError, if the extractor is used with no hole" in {
    a[MatchError] should be thrownBy {
      val Hole(_) = "no hole"
    }
  }

  behavior of "A dotted hole"

  it can "be used to get the number back, which was used to create the dotted hole" in {
    val DottedHole(0)   = "..$hole0"
    val DottedHole(1)   = "..$hole1"
    val DottedHole(2)   = "..$hole2"
    val DottedHole(999) = "..$hole999"
  }

  it should "throw a MatchError, if the extractor is used with no dotted hole" in {
    a[MatchError] should be thrownBy {
      val DottedHole(_) = "no hole"
    }
    a[MatchError] should be thrownBy {
      val DottedHole(0) = "$hole0"
    }
  }

}
