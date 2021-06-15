package me.sakigamiyang.scalambda

import scala.io.StdIn

object LambdaREPL {
  val parser = new LambdaParser

  def main(args: Array[String]): Unit = {
    loop()
  }

  def loop(): Unit = {
    while (true) {
      val exprSrc = StdIn.readLine("λ> ")
      import parser.{NoSuccess, Success}
      parser.parse(exprSrc) match {
        case Success(expr, _) => println("Parsed: " + expr)
        case err: NoSuccess => println(err)
      }
    }
  }
}
