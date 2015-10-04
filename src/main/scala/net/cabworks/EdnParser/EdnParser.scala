package net.cabworks.EdnParser

import clojure.lang.{Keyword, Symbol}

import scala.util.parsing.combinator._
/**
 * Created by cab on 03/10/2015.
 */

object EdnParser extends JavaTokenParsers {

  val list :Parser[List[Any]] = "(" ~> rep(expr) <~ ")"
  val vector : Parser[Vector[Any]] = "[" ~> rep(expr) <~ "]" ^^ (Vector() ++ _)
  val map : Parser[Map[Any,Any]] = "{" ~> rep(pair) <~ "}" ^^ (Map() ++ _)
  val set : Parser[Set[Any]] = "#{" ~> rep(expr) <~ "}" ^^ (Set() ++ _)

  lazy val pair : Parser[(Any, Any)] = expr ~ expr ^^ {
    case k ~ v => (k, v)
  }

  val ratio : Parser[Double] = decimalNumber ~ "/" ~ decimalNumber ^^ { case n ~ _ ~ d => n.toDouble / d.toDouble }

  val keyword : Parser[Keyword] = ":" ~> """[^,#\"\{\}\[\]\s]+""".r ^^ (Keyword.intern(_) )
  val symbol : Parser[Symbol]   = """[a-zA-Z][^,#\"\{\}\[\]\(\)\s]*""".r ^^ (Symbol.create(_))

  val ednElem : Parser[Any] = list |
                map |
                vector |
                set |
                keyword |
                ratio |
           //     wholeNumber <~ not(".") ^^ (_.toInt) |
                floatingPointNumber ^^ (_.toDouble) |
                "nil" ^^ (_ => null) |
                "true" ^^ (_ => true) |
                "false" ^^ (_ => false) |
                symbol |
                stringLiteral ^^ { case "" => ""; case s => s.tail.init}

  val expr : Parser[Any] = ednElem | "," ~> expr | "N" ~> expr



  def eval(input: String) : Any = parseAll(expr, input) match {
    case Failure(msg, n)=> println(msg + " " + n.toString)
    case Error(msg, n) => println("Fatal error" + msg)
    case Success(r,n) => r
  }
}
