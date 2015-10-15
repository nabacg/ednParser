package net.cabworks.test.EdnParser

import clojure.lang.{Keyword, Symbol}
import com.sun.javaws.exceptions.InvalidArgumentException
import net.cabworks.Evaluator.Evaluator
import org.scalatest.FunSuite

/**
 * Created by cab on 14/10/2015.
 */
class EvaluatorTests extends FunSuite {
  def eval(input : String) = Evaluator.evalString(input)

  test("self evaluating") {
    assertResult(1) { eval("1") }
    assertResult("TEST STRING") { eval("\"TEST STRING\"") }
    assertResult(Keyword.intern("d")) { eval(":d")}
    assertResult(42) { eval("a")}



    try {
      eval("b")
      fail()
    }
    catch {
      case _ : InvalidArgumentException =>
      case d : Throwable => fail()
    }

  }

  test("quoted lists") {
    assertResult(List(1, 2, 2)) { eval("'(1 2 2)")}
    assertResult(List(32, 2, 2)) { eval("'(32 2 2)")}
    assertResult(List(Symbol.intern("/"), 2, 2)) { eval("'(/ 2 2)")}
    assertResult(List(Symbol.intern("*"), 6, 7)) { eval("'(* 6 7)")}
  }

  test("calculator") {
    assertResult(4) { eval("(+ 2 2)")}
    assertResult(0) { eval("(- 2 2)")}
    assertResult(1) { eval("(/ 2 2)")}
    assertResult(42) { eval("(* 6 7)")}
  }

  test("nested expressions") {
    assertResult(6) { eval(" (+ (* 2 2) 2)")}
    assertResult(33) { eval(" (+ (* (- 24 10) 2) (/ 15 3))")}

  }

  test("ifs") {
    assertResult("True indeed") { eval("(if true \"True indeed\"  \"Deeply false\")")}
    assertResult("Deeply false") { eval("(if false \"True indeed\"  \"Deeply false\")")}

    assertResult("True indeed") { eval("(if (= 2 2) \"True indeed\"  \"Deeply false\")")}
    assertResult("Deeply false") { eval("(if (not= 2 2) \"True indeed\"  \"Deeply false\")")}


    assertResult("True indeed") { eval("(if (> 3 2) \"True indeed\"  \"Deeply false\")")}
    assertResult("Deeply false") { eval("(if (< 2 2) \"True indeed\"  \"Deeply false\")")}

    assertResult("True indeed") { eval("(if (>= 2 2) \"True indeed\"  \"Deeply false\")")}
    assertResult("True indeed") { eval("(if (>= 4 2) \"True indeed\"  \"Deeply false\")")}
    assertResult("True indeed") { eval("(if (<= 2 2) \"True indeed\"  \"Deeply false\")")}
    assertResult("True indeed") { eval("(if (<= 0 2) \"True indeed\"  \"Deeply false\")")}
    assertResult("Deeply false") { eval("(if (<= 3 2) \"True indeed\"  \"Deeply false\")")}


    assertResult("True indeed") { eval("(if (and true (> 2 1)) \"True indeed\"  \"Deeply false\")")}
    assertResult("Deeply false") { eval("(if (and true (> 2 2)) \"True indeed\"  \"Deeply false\")")}

    assertResult("True indeed") { eval("(if (and true (> 2 1) (< 0 23)) \"True indeed\"  \"Deeply false\")")}
    assertResult("True indeed") { eval("(if (and true (> 2 1) (< 0 23) (= 4 (+ 2 2))) \"True indeed\"  \"Deeply false\")")}
    assertResult("Deeply false") { eval("(if (and true (> 2 1) (< 0 23) (= 4 (+ 2 3))) \"True indeed\"  \"Deeply false\")")}
  }
}
