package me.sakigamiyang.scalambda

class PrettyPrinter {
  def apply(expr: Expr): String = expr match {
    case Lambda(arg, body) => p"λ$arg.$body"
    case Apply(fun, arg) => p"$fun $arg"
    case Var(name, _) => s"$name"
  }

  implicit class PrettyPrinting(val sc: StringContext) {
    def p(args: Expr*): String = sc.s(args.map(parensIfNeeded): _*)
  }

  def parensIfNeeded(expr: Expr): String = expr match {
    case v: Var => apply(v)
    case _ => "(" + apply(expr) + ")"
  }
}
