package me.sakigamiyang.scalambda

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/**
 * λ-calculus EBNF:
 * expr        = lambda | application | variable | parens;
 * lambda      = "λ", variable, ".", expr;
 * application = expr, expr;
 * variable    = identifier;
 * parens      = "(", expr, ")";
 */
class LambdaParser extends StdTokenParsers with PackratParsers {
  override type Tokens = StdLexical
  override val lexical = new LambdaLexer
  lexical.delimiters ++= Seq("\\", "λ", ".", "(", ")", "=", ";")

  type P[+T] = PackratParser[T]
  lazy val expr: P[Expr] = application | notApp
  // lazy val notApp: Parser[Expr] = variable | number | parens | lambda
  lazy val notApp: P[Expr] = variable | number | parens | lambda
  lazy val lambda: P[Lambda] = positioned(("λ" | "\\") ~> variable ~ "." ~ expr
    ^^ { case v ~ "." ~ e => Lambda(v, e) })
  lazy val application: P[Apply] = positioned(expr ~ notApp
    ^^ { case left ~ right => Apply(left, right) })
  lazy val variable: P[Var] = positioned(ident ^^ Var.apply)
  lazy val parens: P[Expr] = "(" ~> expr <~ ")"
  lazy val number: P[Lambda] = numericLit ^^ (n => CNumber(n.toInt))

  def parse(s: String): ParseResult[Expr] = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }

  lazy val defs: Parser[Map[String, Expr]] = repsep(defn, ";") <~ opt(";") ^^ Map().++
  lazy val defn: Parser[(String, Expr)] = ident ~ "=" ~ expr ^^ { case id ~ "=" ~ t => id -> t }

  def definitions(s: String): ParseResult[Map[String, Expr]] = {
    val tokens = new lexical.Scanner(s)
    phrase(defs)(tokens)
  }
}

class LambdaLexer extends StdLexical {
  override def letter: Parser[Char] = elem("letter", c => c.isLetter && c != 'λ')
}
