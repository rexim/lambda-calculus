package me.rexim.morganey.ast

sealed trait LambdaTerm extends MorganeyNode {
  def substitute(substitution : (LambdaVar, LambdaTerm)): LambdaTerm

  /**
    * Tells whether this term contains v as a free variable
    */
  def containsFreeVar(v: LambdaVar): Boolean

  def addContext(context: Seq[MorganeyBinding]): LambdaTerm =
    context.foldRight(this) {
      case (MorganeyBinding(variable, value), acc) =>
        LambdaApp(LambdaFunc(variable, acc), value)
    }
}

case class LambdaVar(name: String) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (name == v.name) r else this
  }

  override def containsFreeVar(v: LambdaVar): Boolean = v == this

  override def toString = name
}

case class LambdaFunc(parameter: LambdaVar, body: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (parameter == v) {
      this
    } else if (r.containsFreeVar(parameter)) {
      val newParameter =
        Stream.from(0)
          .map(number => LambdaVar(s"${parameter.name}##$number"))
          .dropWhile(x => r.containsFreeVar(x) || body.containsFreeVar(x))
          .head

      val newBody = body.substitute(parameter -> newParameter)
      LambdaFunc(newParameter, newBody.substitute(v -> r))
    } else {
      LambdaFunc(parameter, body.substitute(v -> r))
    }
  }

  override def containsFreeVar(v: LambdaVar): Boolean =
    parameter != v && body.containsFreeVar(v)

  override def toString = s"(λ ${parameter.name} . $body)"
}

case class LambdaApp(leftTerm: LambdaTerm, rightTerm: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    LambdaApp(
      leftTerm.substitute(v -> r),
      rightTerm.substitute(v -> r))
  }

  override def containsFreeVar(v: LambdaVar): Boolean =
    leftTerm.containsFreeVar(v) || rightTerm.containsFreeVar(v)

  override def toString = s"($leftTerm $rightTerm)"
}
