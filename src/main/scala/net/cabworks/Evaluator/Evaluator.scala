package net.cabworks.Evaluator

import clojure.lang.{Keyword, Symbol}
import com.sun.javaws.exceptions.InvalidArgumentException
import net.cabworks.EdnParser.EdnParser
import Numeric.Implicits._

/**
 * Created by cab on 14/10/2015.
 */
object Evaluator {

  def evalString(input : String) : Any = eval(EdnParser.read(input))

  def eval(e : Any) : Any = e match {
    case b : Boolean => b
    case n : Number => n
    case s : String => s
    case k : Keyword => k
    case s : Symbol => lookupSymbol(s)
    case ifsym :: pred :: thenExpr :: elseExpr :: _ if ifsym == Symbol.intern("if") => eval(pred) match { case true => eval(thenExpr);
                                                                                                          case false => eval(elseExpr)}
   // case "if" :: tail => eval(tail.head) match {case true => eval(tail.tail.head); case false => eval(tail.tail.tail.head)}
    case (h :: tail) if h == Symbol.intern("quote") => tail.head
    case (f :: args)=> apply(eval(f).asInstanceOf[(Any) => Any], args.map(eval))

  }

  def apply[T, R](f : (T) => R, args : T) = f(args)
//  def apply1[T, R]( args : T*) = println(args : _*)

  def numericBinaryOperator[T](op : (T , T) => T)(args : Seq[T]) : T = args.reduce(op) //args.tail.foldLeft(args.head)((a, i) => op(a, i))
  def boolBinaryOperator[T](op : (T, T) => Boolean)(args : Seq[T]) : Boolean = args match {
      case l :: r :: Nil => op(l, r)
      case _ => throw new InvalidArgumentException(Array(s"Can't apply arguments: $args to operator: $op"))
    }

  def lookupSymbol(s : Symbol) : Any = environment.get(s) match {
    case Some(a ) => a
    case None => throw new InvalidArgumentException(Array("Unknown symbol " + s.toString + " make sure value is declared before using it!"))
  }
//  def numericAdd[N : Numeric](a : N, b : N)(implicit  num: Numeric[N]) = num.plus(a, b)
  def numericAdd[N : Numeric](a : N, b : N) =  a + b //implicitly[Numeric[N]].plus(a, b)
  def numericDiff(a : Any, b : Any) (implicit  n : Numeric[Any]) = n.plus(a, n.negate(b))

  class numericAddition [N : Numeric] extends Function2[N, N, N] {
    def apply(a: N, b : N) : N = a + b
  }

  val environment: Map[Symbol, Any] = Map(Symbol.intern("a") -> 42,
    Symbol.intern("+") -> numericBinaryOperator((a : Int, b : Int) => this.numericAdd(a, b))_,
//    Symbol.intern("+") -> numericBinaryOperator(new numericAddition[Int])_,
//    Symbol.intern("-") -> numericBinaryOperator((a : Any, b : Any) => this.numericDiff(a, b))_,
    Symbol.intern("-") -> numericBinaryOperator((a : Int, b : Int) => a - b)_,
    Symbol.intern("*") -> numericBinaryOperator((a : Int, b : Int) => a * b)_,
    Symbol.intern("/") -> numericBinaryOperator((a : Int, b : Int) => a / b)_,
    Symbol.intern("=") -> boolBinaryOperator((a : Int, b : Int ) => a == b)_,
    Symbol.intern("not=") -> boolBinaryOperator((a : Int , b : Int) => a != b)_,
    Symbol.intern("not=") -> boolBinaryOperator((a : Int , b : Int) => a != b)_,
    Symbol.intern("<") -> boolBinaryOperator((a : Int , b : Int) => a < b)_,
    Symbol.intern("<=") -> boolBinaryOperator((a : Int , b : Int) => a <= b)_,
    Symbol.intern(">") -> boolBinaryOperator((a : Int , b : Int) => a > b)_,
    Symbol.intern(">=") -> boolBinaryOperator((a : Int , b : Int) => a >= b)_,
    Symbol.intern("and") -> numericBinaryOperator((a : Boolean , b : Boolean) => a && b)_,
    Symbol.intern("or") -> numericBinaryOperator((a : Boolean , b : Boolean) => a || b)_
  )  // this is quite ridiculous though.. // todo find a way to make it work on Numerics without hardcoded ints!

  //((xs : Seq[Int]) => numericBinaryOperator((a: Int, b: Int) => this.numericAdd(a, b))(xs))
}
