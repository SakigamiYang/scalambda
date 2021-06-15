package me.sakigamiyang.scalambda

import scala.collection.mutable.ListBuffer

object Scope {
  var id = 0

  def nextId: Int = {
    val i = id
    id += 1
    i
  }

  val TOP = new Scope(None, Set())
}

class Scope(val parent: Option[Scope], val boundNames: Set[String]) {
  val id: Int = Scope.nextId

  def closestBinding(name: String): Option[Scope] =
    if (boundNames.contains(name))
      Some(this)
    else
      parent.flatMap(_.closestBinding(name))
}

class Binder(val defs: Map[String, Expr]) {
  val messages: ListBuffer[Message] = ListBuffer[Message]()

  def apply(term: Expr): Expr = bind(term, Scope.TOP)

  def bind(term: Expr, parent: Scope): Expr = term match {
    case Lambda(arg, body) =>
      val lambdaScope = new Scope(Some(parent), Set(arg.name))
      Lambda(arg.copy(scope = lambdaScope), bind(body, lambdaScope))
    case v@Var(name, _) if defs.contains(name) =>
      bind(defs(name), parent)
    case v@Var(name, _) =>
      parent.closestBinding(name) match {
        case Some(scope) =>
          v.copy(scope = scope)
        case None if name(0).isUpper =>
          v.copy(scope = Scope.TOP)
        case None =>
          messages += Message(v.pos, "Unbound variable: " + name)
          v
      }
    case Apply(fun, arg) =>
      Apply(bind(fun, parent), bind(arg, parent))
  }

}
