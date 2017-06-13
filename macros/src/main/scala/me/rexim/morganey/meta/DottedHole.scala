package me.rexim.morganey.meta

private[meta] object DottedHole {

  def unapply(arg: String): Option[Int] =
    if (arg startsWith "..") Hole.unapply(arg stripPrefix "..")
    else None

}
