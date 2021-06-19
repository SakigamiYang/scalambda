package me.sakigamiyang.scalambda

import me.sakigamiyang.scalambda.eval.Evaluation

import scala.io.StdIn

object LambdaREPL {
  val parser = new LambdaParser
  val pretty = new PrettyPrinter
  var bind = new Binder(Library.load())
  var eval = new Evaluation(debug = false)

  def main(args: Array[String]): Unit = {
    loop()
  }

  def loop(): Unit = {
    while (true) {
      val input = StdIn.readLine("λ> ")
      if (input startsWith ":")
        handleCommand(input substring 1)
      else if (input contains "=")
        handleDef(input)
      else
        handleExpr(input)
    }
  }

  def handleCommand(input: String): Unit = input.split(" ", 2) match {
    case Array("step" | "s", exprSrc) =>
      parseInput(parser.parse, exprSrc) { expr =>
        try {
          val in = bind(expr)
          println(pretty(in) + " →")
          val out = eval.evalStep(in)
          println(pretty(out))
        } catch {
          case _: MatchError => println("Cannot reduce further.")
        }
      }
    case Array("debug" | "d", arg) =>
      arg match {
        case "on" => eval = new Evaluation(debug = true)
        case "off" => eval = new Evaluation(debug = false)
        case _ => println("Syntax: ':debug off' or ':debug on'")
      }
    case Array("quit" | "q") =>
      System.exit(0)
    case cmd =>
      println("Unknown command: " + (cmd mkString " "))
  }

  def handleDef(input: String): Unit =
    parseInput(parser.definitions, input) { defs =>
      println("Defined: " + defs.keys.mkString(", "))
      bind = new Binder(bind.defs ++ defs)
    }

  def handleExpr(input: String): Unit =
    parseInput(parser.parse, input) { expr =>
      val bound = bind(expr)
      if (bind.messages.isEmpty)
        println(pretty(eval(bound)))
      else {
        for (m <- bind.messages)
          println(m.pos.longString + m.msg)
        bind.messages.clear()
      }
    }

  def parseInput[T](p: String => parser.ParseResult[T], input: String)(success: T => Unit): Unit = {
    import parser.{NoSuccess, Success}
    p(input) match {
      case Success(res, _) => success(res)
      case NoSuccess(err, _) => println(err)
    }
  }


}
