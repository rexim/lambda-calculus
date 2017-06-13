package me.rexim.morganey.meta

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest.{FlatSpec, Matchers}

class InterpolatorSpec extends FlatSpec with Matchers with TestTerms {

  "The interpolator" should "create valid lambda variables" in {
    m"x" should be (x)
    m"y" should be (y)
    m"z" should be (z)
  }

  it should "create simple valid lambda applications" in {
    m"(x x)" should be (LambdaApp(x, x))
    m"(y z)" should be (LambdaApp(y, z))
  }

  it should "create nested valid lambda applications" in {
    m"(x (y z))" should be (LambdaApp(x, LambdaApp(y, z)))
    m"((x y) z)" should be (LambdaApp(LambdaApp(x, y), z))

    m"((b c) (d e))" should be (LambdaApp(LambdaApp(b, c), LambdaApp(d, e)))
  }


  "Lambda terms" can "be composed using the interpolator" in {
    val body = LambdaApp(b, c)

    m"($b $c)" should be (body)
    m"(\\$b . $body)" should be (LambdaFunc(b, body))
    m"(λ$b . $body)" should be (LambdaFunc(b, body))
  }


  "Scala values" should "be converted automatically to lambda terms during lifting" in {
    val nZero = 0
    m"$nZero" should be (zero)

    val nOne = 1
    m"$nOne" should be (one)

    val nTwo = 2
    m"$nTwo" should be (two)

    val numbers = List(0, 1, 2)
    m"$numbers" should be (pair(zero, pair(one, pair(two, zero, "z"), "z"), "z"))
  }

  "Matching against big strings" should "be possible with the quotation mechanism" in {
    val string = "The quick brown fox jumps over the lazy dog"
    // `string` is represented by 4057 applications in morganey
    val m""" "The quick brown fox jumps over the lazy dog" """ = m"$string"
  }

  "Lambda terms" should "be converted back to Scala values during unlifting" in {
    val m"${nZero: Int}" = zero
    nZero should be (0)

    val m"${nOne: Int}" = one
    nOne should be (1)

    val m"${nTwo: Int}" = two
    nTwo should be (2)

    val m"${numbers: List[Int]}" = `[0 .. 2]`
    numbers should be (List(0, 1, 2))

    val m"${list: List[Int]}" = zero
    list.isEmpty should be (true)
  }

  "Extraction of subtrees of applications" should "be supported by the unquotation macro" in {
    val m"$wholeApp" = m"b c d"
    wholeApp should be (lapp(lapp(b, c), d))

    val m"$fstApp1 d" = m"b c d"
    fstApp1 should be (lapp(b, c))

    val m"$fstApp2 d" = m"c d"
    fstApp2 should be (c)

    val m"c $sndApp" = m"c d"
    sndApp should be (d)
  }

  "Extraction of subtrees of abstractions" should "be supported by the unquotation macro" in {
    val m"$wholeApp" = m"\\b.b"
    wholeApp should be (I(b))

    val m"\\$fstArg.c.d.1" = m"\\b.c.d.1"
    fstArg should be (b)

    val m"\\b.$sndArg.d.1" = m"\\b.c.d.1"
    sndArg should be (c)

    val m"\\b.c.$trdArg.1" = m"\\b.c.d.1"
    trdArg should be (d)

    val m"\\b.c.d.$body" = m"\\b.c.d.1"
    body should be (one)

    val m"\\b.c.$rest" = m"\\b.c.d.1"
    rest should be (lfunc("d", one))
  }

  "Splicing sequences of terms into lists" should "be supported by the quotation macro" in {
    // Lists
    {
      val terms   = List(zero, one, two)
      val spliced = m"[..$terms]"
      spliced should be (`[0 .. 2]`)
    }
    {
      val terms   = List(one, two)
      val spliced = m"[0, ..$terms]"
      spliced should be (`[0 .. 2]`)
    }
    {
      val terms   = List(zero, one)
      val spliced = m"[..$terms, 2]"
      spliced should be (`[0 .. 2]`)
    }

    // Vectors
    {
      val terms   = Vector(zero, one, two)
      val spliced = m"[..$terms]"
      spliced should be (`[0 .. 2]`)
    }
    {
      val terms   = Vector(one, two)
      val spliced = m"[0, ..$terms]"
      spliced should be (`[0 .. 2]`)
    }
    {
      val terms   = Vector(zero, one)
      val spliced = m"[..$terms, 2]"
      spliced should be (`[0 .. 2]`)
    }

    // Seqs
    {
      val terms   = Seq(zero, one, two)
      val spliced = m"[..$terms]"
      spliced should be (`[0 .. 2]`)
    }
    {
      val terms   = Seq(one, two)
      val spliced = m"[0, ..$terms]"
      spliced should be (`[0 .. 2]`)
    }
    {
      val terms   = Seq(zero, one)
      val spliced = m"[..$terms, 2]"
      spliced should be (`[0 .. 2]`)
    }
  }

  "Unsplicing sequences of terms out of lists (without specified types)" should "be supported by the unquotation macro" in {
    {
      val m"[..$unspliced]" = `[0 .. 2]`
      unspliced should be(List(zero, one, two))
    }
    {
      val m"[0, ..$unspliced]" = `[0 .. 2]`
      unspliced should be(List(one, two))
    }
    {
      val m"[..$unspliced, 2]" = `[0 .. 2]`
      unspliced should be(List(zero, one))
    }
  }

  "Unsplicing sequences of terms out of lists (with specified types)" should "be supported by the unquotation macro" in {
    // Lists
    {
      val m"[..${unspliced: List[LambdaTerm]}]" = `[0 .. 2]`
      unspliced should be(List(zero, one, two))
    }
    {
      val m"[0, ..${unspliced: List[LambdaTerm]}]" = `[0 .. 2]`
      unspliced should be(List(one, two))
    }
    {
      val m"[..${unspliced: List[LambdaTerm]}, 2]" = `[0 .. 2]`
      unspliced should be(List(zero, one))
    }

    // Vectors
    {
      val m"[..${unspliced: Vector[LambdaTerm]}]" = `[0 .. 2]`
      unspliced should be (Vector(zero, one, two))
    }
    {
      val m"[0, ..${unspliced: Vector[LambdaTerm]}]" = `[0 .. 2]`
      unspliced should be (Vector(one, two))
    }
    {
      val m"[..${unspliced: Vector[LambdaTerm]}, 2]" = `[0 .. 2]`
      unspliced should be (Vector(zero, one))
    }

    // Seqs
    {
      val m"[..${unspliced: Seq[LambdaTerm]}]" = `[0 .. 2]`
      unspliced should be (Seq(zero, one, two))
    }
    {
      val m"[0, ..${unspliced: Vector[LambdaTerm]}]" = `[0 .. 2]`
      unspliced should be (Seq(one, two))
    }
    {
      val m"[..${unspliced: Vector[LambdaTerm]}, 2]" = `[0 .. 2]`
      unspliced should be (Seq(zero, one))
    }
  }

  "Unsplicing sequences of terms out of lists (with specified types and autoconversion)" should "be supported by the unquotation macro" in {
    // Lists
    {
      val m"[..${unspliced: List[Int]}]" = `[0 .. 2]`
      unspliced should be(List(0, 1, 2))
    }
    {
      val m"[0, ..${unspliced: List[Int]}]" = `[0 .. 2]`
      unspliced should be(List(1, 2))
    }
    {
      val m"[..${unspliced: List[Int]}, 2]" = `[0 .. 2]`
      unspliced should be(List(0, 1))
    }

    // Vectors
    {
      val m"[..${unspliced: Vector[Int]}]" = `[0 .. 2]`
      unspliced should be (Vector(0, 1, 2))
    }
    {
      val m"[0, ..${unspliced: Vector[Int]}]" = `[0 .. 2]`
      unspliced should be (Vector(1, 2))
    }
    {
      val m"[..${unspliced: Vector[Int]}, 2]" = `[0 .. 2]`
      unspliced should be (Vector(0, 1))
    }

    // Seqs
    {
      val m"[..${unspliced: Seq[Int]}]" = `[0 .. 2]`
      unspliced should be (Seq(0, 1, 2))
    }
    {
      val m"[0, ..${unspliced: Vector[Int]}]" = `[0 .. 2]`
      unspliced should be (Seq(1, 2))
    }
    {
      val m"[..${unspliced: Vector[Int]}, 2]" = `[0 .. 2]`
      unspliced should be (Seq(0, 1))
    }
  }

}
