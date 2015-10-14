package net.cabworks.test.EdnParser

import clojure.lang.Keyword
import com.sun.javaws.exceptions.InvalidArgumentException
import net.cabworks.Evaluator.Evaluator
import org.scalatest.FunSuite

/**
 * Created by cab on 14/10/2015.
 */
class EvaluatorTests extends FunSuite {
  def eval(input : String) = Evaluator.eval(input)

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

  test("calculator") {
    assertResult(4) { eval("(+ 2 2)")}
    assertResult(0) { eval("(- 2 2)")}
    assertResult(1) { eval("(/ 2 2)")}
    assertResult(42) { eval("(* 6 7)")}
  }

  test("nested expressions") {
    assertResult(6) { eval(" (+ (* 2 2) 2)")}
    assertResult(6) { eval(" (+ (* (- 24 10) 2) (/ 15 3))")}

  }


}
