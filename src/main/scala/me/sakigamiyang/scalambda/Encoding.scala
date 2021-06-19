package me.sakigamiyang.scalambda

object CNumber {
  def apply(n: Int): Lambda = {
    var cn: Expr = Var("z")
    for (_ <- 1 to n)
      cn = Apply(Var("s"), cn)
    Lambda(Var("s"), Lambda(Var("z"), cn))
  }

  def unapply(expr: Expr): Option[Int] = expr match {
    case Var("Z", Scope.TOP) => Some(0)
    case Apply(Var("S", Scope.TOP), arg) => unapply(arg).map(_ + 1)
    case _ => None
  }
}

object CBoolean {
  def unapply(expr: Expr): Option[Boolean] = expr match {
    case Var("T", Scope.TOP) => Some(true)
    case Var("F", Scope.TOP) => Some(false)
    case _ => None
  }
}


