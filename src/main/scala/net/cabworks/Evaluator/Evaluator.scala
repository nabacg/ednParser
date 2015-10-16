package net.cabworks.Evaluator

import clojure.lang.{Keyword, Symbol}
import net.cabworks.EdnParser.EdnParser
import Numeric.Implicits._

/**
 * Created by cab on 14/10/2015.
 */
object Evaluator {

  def evalString(input : String) : Any = eval(initEnv, EdnParser.read(input))

  def defineFunction(params: Vector[Symbol], body: Any): Any = (params, body)

  def eval(env : Map[Symbol,Any], e : Any) : Any = e match {
    case b : Boolean => b
    case n : Number => n
    case s : String => s
    case k : Keyword => k
    case s : Symbol => lookupSymbol(env, s)
    case ifSym :: pred :: thenExpr :: elseExpr :: _ if ifSym == Symbol.intern("if") => eval(env, pred) match { case true => eval(env, thenExpr);
                                                                                                          case false => eval(env, elseExpr)}
    case (defSym : Symbol) :: (nameSym : Symbol) :: value ::  _ if defSym == Symbol.intern("def") => updateEnv(nameSym, eval(env, value))
    case (fn : Symbol) :: (params : Vector[Symbol]) :: body :: _ if fn == Symbol.intern("fn") => defineFunction(params, body)
    case (h :: tail) if h == Symbol.intern("quote") => tail.head
//    case (f :: args)=> apply(eval(env, f).asInstanceOf[(Any) => Any], args.map(eval(env, _)))
    case ((f : Symbol) :: args)=> apply(f, args, env)

  }

  def apply(f : Symbol, args : Seq[Any], env : Map[Symbol, Any]) : Any = f match {
    case f if primitiveProcedures.contains(f) => apply(eval(env, f).asInstanceOf[(Any) => Any], args.map(eval(env, _)))
    case f => {
//      val (params, body) = env.get(f)
//      eval(extendEnv(env, params, args), body)
      env.get(f) match {
        case Some((params : Seq[Symbol], body)) => eval(extendEnv(env, params, args), body)
        case None => sys.error(s"Unknown symbol $f make sure value is declared before using it!")
        case _ => sys.error(s"Symbol $f is not a function or primitive operator, are you trying to call a non function symbol?")
      }
    }
  }

  def extendEnv(env : Map[Symbol, Any], ks : Seq[Symbol], vals : Seq[Any]) = env ++ ks.zip(vals)

  def apply[T, R](f : (T) => R, args : T) = f(args)
//  def apply1[T, R]( args : T*) = println(args : _*)

  def initEnv: Map[Symbol, Any] = {
    environment = primitiveProcedures ++ environment
    environment
  }
  def updateEnv(k : Symbol, v : Any) = {
    environment = environment + (k -> v)
    v
  }

  def numericBinaryOperator[T](op : (T , T) => T)(args : Seq[T]) : T = args.reduce(op) //args.tail.foldLeft(args.head)((a, i) => op(a, i))
  def boolBinaryOperator[T](op : (T, T) => Boolean)(args : Seq[T]) : Boolean = args match {
      case l :: r :: Nil => op(l, r)
      case _ => sys.error(s"Can't apply arguments: $args to operator: $op")
    }

  def lookupSymbol(env : Map[Symbol, Any], s : Symbol) : Any = env.get(s) match {
    case Some(a ) => a
    case None => sys.error("Unknown symbol " + s.toString + " make sure value is declared before using it!")
  }
//  def numericAdd[N : Numeric](a : N, b : N)(implicit  num: Numeric[N]) = num.plus(a, b)
  def numericAdd[N : Numeric](a : N, b : N) =  a + b //implicitly[Numeric[N]].plus(a, b)
  def numericDiff(a : Any, b : Any) (implicit  n : Numeric[Any]) = n.plus(a, n.negate(b))

  class numericAddition [N : Numeric] extends Function2[N, N, N] {
    def apply(a: N, b : N) : N = a + b
  }

  var environment : Map[Symbol, Any] = Map(Symbol.intern("a") -> 42)

  val primitiveProcedures: Map[Symbol, Any] = Map(
    Symbol.intern("+") -> numericBinaryOperator((a : Int, b : Int) => this.numericAdd(a, b))_,
//    Symbol.intern("+") -> numericBinaryOperator(this.numericAdd)_,
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
