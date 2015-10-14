package net.cabworks.Evaluator

import clojure.lang.{Keyword, Symbol}
import com.sun.javaws.exceptions.InvalidArgumentException
import net.cabworks.EdnParser.EdnParser
import Numeric.Implicits._

/**
 * Created by cab on 14/10/2015.
 */
object Evaluator {


  def eval(input : String) : Any = {
    val edn = EdnParser.read(input)

    def evalInner(e : Any) : Any = e match {
      case n : Number => n
      case s : String => s
      case k : Keyword => k
      case s : Symbol => lookupSymbol(s)
      case (f :: args)=> apply(evalInner(f).asInstanceOf[(Any) => Any], args.map(evalInner))

    }
    evalInner(edn)
  }

  def apply[T, R](f : (T) => R, args : T) = f(args)
//  def apply1[T, R]( args : T*) = println(args : _*)

  def numericBinaryOperator[T](op : (T , T) => T)(args : Seq[T]) : T = args.tail.foldLeft(args.head)((a, i) => op(a, i))

  def lookupSymbol(s : Symbol) : Any = environment.get(s) match {
    case Some(a ) => a
    case None => throw new InvalidArgumentException(Array("Unknown symbol " + s.toString + " make sure value is declared before using it!"))
  }
//  def numericAdd[N : Numeric](a : N, b : N)(implicit  num: Numeric[N]) = num.plus(a, b)
  def numericAdd[N : Numeric](a : N, b : N) =  a + b //implicitly[Numeric[N]].plus(a, b)

  val environment: Map[Symbol, Any] = Map(Symbol.intern("a") -> 42,
    Symbol.intern("+") -> numericBinaryOperator((a : Int, b : Int) => this.numericAdd(a, b))_,
    Symbol.intern("-") -> numericBinaryOperator((a : Int, b : Int) => a - b)_,
    Symbol.intern("*") -> numericBinaryOperator((a : Int, b : Int) => a * b)_,
    Symbol.intern("/") -> numericBinaryOperator((a : Int, b : Int) => a / b)_)  // this is quite ridiculous though.. // todo find a way to make it work on Numerics without hardcoded ints!
  //((xs : Seq[Int]) => numericBinaryOperator((a: Int, b: Int) => this.numericAdd(a, b))(xs))
}
