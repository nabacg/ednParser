package net.cabworks.test.EdnParser



import org.scalatest.FunSuite
import net.cabworks.EdnParser.{InstantReader, EdnParser}
import clojure.lang.{Keyword, Symbol}
import java.util.UUID
/**
 * Created by cab on 03/10/2015.
 */

class ParserTest extends FunSuite{
  def testEval (str : String) = EdnParser.eval(str)
  
  test("integers") {
    assertResult(0){testEval("0")}
    assertResult(1){testEval("0001")}
    assertResult(23) {testEval("23")}
    assertResult(1) {testEval("1")}
    assertResult(-43) { testEval("-43")}
  }
  test("floating point") {
    assertResult(53.1) {testEval("53.1")}
    assertResult(1.5) {testEval("1.5")}
    assertResult(0.0005) {testEval("0.0005")}
    assertResult(0.5) {testEval("0.5000")}
    assertResult(-3.14) {testEval("-3.14")}
  }

  test("nil ") {
    assertResult(null) { testEval("nil")}
  }

  test("booleans") {
    assertResult(true) {testEval("true")}
    assertResult(false) { testEval("false")}
  }
  
  test("string") {
    assertResult("1") { testEval("\"1\"")}
    assertResult("AA") { testEval("\"AA\"")}
    assertResult("") { testEval("\"\"")}
  }

  test("ratios") {
    assertResult(0.5) {testEval("1/2")}
    assertResult(0.5) {testEval("6/12")}
    assertResult(Set(0.5, 1)) { testEval("#{1/2 42/42}") }
  }

  test("dates aka #inst") {
    val dateString = "2015-07-30T01:23:45.000-00:00"
    assertResult(InstantReader.read(dateString)) {testEval(f"""#inst \"$dateString\"""")}

    val dateStringNoOffset = "1985-04-12T23:20:50.52Z"
    assertResult(InstantReader.read(dateString)) {testEval(f"""#inst \"$dateString\"""")}
  }

  test("#uuid tagged elem") {
    var uuid = UUID.randomUUID()
    assertResult(uuid) { testEval(s"""#uuid \"${uuid.toString}\"""") }
    assertResult(UUID.fromString("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")) { testEval("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")}
  }

  test("List") {
    assertResult(List()){testEval("()")}
    assertResult(List(1,2,3)) {testEval("(1 2 3)")}
    assertResult(List(1,2,3)) {testEval("(1,2,3)")}
  }

  test("vectors ") {
    assertResult(Vector(1)) { testEval("[1]")}
    assertResult(Vector(1,2, 3)) { testEval("[1 2 3]")}
    assertResult(Vector()) { testEval("[]")}
    assertResult(Vector(1,2,3)) {testEval("[1,2,3]")}
  }

  test("maps") {
    assertResult(Map(1 -> 12))(testEval("{ 1 12}") )

    assertResult(Map(1 -> 12, 2 -> 231))( testEval("{ 1 12 2 231}"))
    assertResult(Map())( testEval("{}"))
  }

  test("sets") {
    assertResult(Set(1, 2, 3))(testEval("#{1 2 3}") )

    assertResult(Set())( testEval("#{}"))
  }


  test("clojure keywords and symbols") {
    assertResult(Symbol.intern("a")) { testEval("a") }
    assertResult(Symbol.intern("f")) { testEval("f") }
    assertResult(Symbol.intern("test-namespace/state")) { testEval("test-namespace/state") }

    assertResult(Keyword.intern("a")) { testEval(":a") }
    assertResult(Keyword.intern(":f")) { testEval("::f") }
    assertResult(Keyword.intern("test/asd")) { testEval(":test/asd") }

    assertResult(List("a", "b", "c", "d").map(Keyword.intern)) { testEval("(:a :b :c :d )")}

    assertResult(List("a", "b", "c", "d").map(Symbol.intern)) { testEval("(a b c d )")}
    assertResult(Vector("a", "b", "c", "d").map(Symbol.intern)) { testEval("[a b c d ]")}
  }
}
