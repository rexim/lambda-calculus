package me.rexim.morganey.meta

private[meta] object Hole {

  private val holePattern   = "$hole"
  private val holeRegex     = "\\$hole([0-9]+)".r

  def apply(n: Int): String =
    s"$holePattern$n"

  def unapply(x: String): Option[Int] =
    Option(x) collect {
      case holeRegex(n) => n.toInt
    }

}
