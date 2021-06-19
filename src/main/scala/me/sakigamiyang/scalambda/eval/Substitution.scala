package me.sakigamiyang.scalambda.eval

import me.sakigamiyang.scalambda._

class Substitution(argVar: Var, replacement: Expr) {
  val binder = new Binder(Map())

  def apply(term: Expr): Expr = term match {
    case Var(argVar.name, argVar.scope) => binder.bind(replacement, argVar.scope.parent.get)
    case Var(_, _) => term
    case Apply(fun, arg) => Apply(apply(fun), apply(arg))
    case Lambda(arg, body) => Lambda(arg, apply(body))
  }
}
