package net.cabworks.test.EdnParser



import org.scalatest.FunSuite
import net.cabworks.EdnParser.{InstantReader, EdnParser}
import clojure.lang.{Keyword, Symbol}
import java.util.UUID
/**
 * Created by cab on 03/10/2015.
 */

class ParserTest extends FunSuite{
  def testParser (str : String) = EdnParser.read(str)
  
  test("integers") {
    assertResult(0){testParser("0")}
    assertResult(1){testParser("0001")}
    assertResult(23) {testParser("23")}
    assertResult(1) {testParser("1")}
    assertResult(-43) { testParser("-43")}
  }
  test("floating point") {
    assertResult(53.1) {testParser("53.1")}
    assertResult(1.5) {testParser("1.5")}
    assertResult(0.0005) {testParser("0.0005")}
    assertResult(0.5) {testParser("0.5000")}
    assertResult(-3.14) {testParser("-3.14")}
  }

  test("equality") {
    assertResult(true) {
      val i = testParser("2")
      val d = testParser("2.0000000000000")
      i == d
    }

    assertResult(false) {
      val i = testParser("2")
      val d = testParser("2.0000000000001")
      i == d
    }
  }

//  test("discard element") {
//    assertResult(Vector(1,2,3)) { testEval("[1 2 #_discardMe 3]")}
//  }

  test("nil ") {
    assertResult(null) { testParser("nil")}
  }

  test("booleans") {
    assertResult(true) { testParser("true")}
    assertResult(false) { testParser("false")}
  }
  
  test("string") {
    assertResult("1") { testParser("\"1\"")}
    assertResult("AA") { testParser("\"AA\"")}
    assertResult("") { testParser("\"\"")}
  }

  test("ratios") {
    assertResult(0.5) { testParser("1/2")}
    assertResult(0.5) { testParser("6/12")}
    assertResult(Set(0.5, 1)) { testParser("#{1/2 42/42}") }
  }

  test("dates aka #inst") {
    val dateString = "2015-07-30T01:23:45.000-00:00"
    assertResult(InstantReader.read(dateString)) { testParser(f"""#inst \"$dateString\"""")}

    val dateStringNoOffset = "1985-04-12T23:20:50.52Z"
    assertResult(InstantReader.read(dateString)) {testParser(f"""#inst \"$dateString\"""")}
  }

  test("#uuid tagged elem") {
    var uuid = UUID.randomUUID()
    assertResult(uuid) { testParser(s"""#uuid \"${uuid.toString}\"""") }
    assertResult(UUID.fromString("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")) { testParser("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")}
  }

  test("List") {
    assertResult(List()){testParser("()")}
    assertResult(List(1,2,3)) {testParser("(1 2 3)")}
    assertResult(List(1,2,3)) {testParser("(1,2,3)")}
  }

  test("vectors ") {
    assertResult(Vector(1)) { testParser("[1]")}
    assertResult(Vector(1,2, 3)) { testParser("[1 2 3]")}
    assertResult(Vector()) { testParser("[]")}
    assertResult(Vector(1,2,3)) {testParser("[1,2,3]")}
  }

  test("maps") {
    assertResult(Map(1 -> 12))(testParser("{ 1 12}") )

    assertResult(Map(1 -> 12, 2 -> 231))( testParser("{ 1 12 2 231}"))
    assertResult(Map())( testParser("{}"))
  }

  test("sets") {
    assertResult(Set(1, 2, 3))(testParser("#{1 2 3}") )

    assertResult(Set())( testParser("#{}"))
  }


  test("clojure keywords and symbols") {
    assertResult(Symbol.intern("a")) { testParser("a") }
    assertResult(Symbol.intern("f")) { testParser("f") }
    assertResult(Symbol.intern("test-namespace/state")) { testParser("test-namespace/state") }

    assertResult(Keyword.intern("a")) { testParser(":a") }
    assertResult(Keyword.intern(":f")) { testParser("::f") }
    assertResult(Keyword.intern("test/asd")) { testParser(":test/asd") }

    assertResult(List("a", "b", "c", "d").map(Keyword.intern)) { testParser("(:a :b :c :d )")}

    assertResult(List("a", "b", "c", "d").map(Symbol.intern)) { testParser("(a b c d )")}
    assertResult(Vector("a", "b", "c", "d").map(Symbol.intern)) { testParser("[a b c d ]")}
  }

  test("arithmetic") {
    assertResult(List(Symbol.intern("+"), 2, 2)) {
      testParser("(+ 2 2)")
    }
    assertResult(List(Symbol.intern("-"), 2, 2)) {
      testParser("(- 2 2)")
    }
    assertResult(List(Symbol.intern("/"), 2, 2)) {
      testParser("(/ 2 2)")
    }
    assertResult(List(Symbol.intern("*"), 2, 2)) {
      testParser("(* 2 2)")
    }
  }

  test("quoted lists") {
    assertResult(List(Symbol.intern("quote"), List(1, 2, 2))) {
      testParser("'(1 2 2)")
    }
    assertResult(List(Symbol.intern("quote"), List(Symbol.intern("+"), 2, 2))) {
      testParser("'(+ 2 2)")
    }

  }
}
