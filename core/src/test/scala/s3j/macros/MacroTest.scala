package s3j.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.annotations.*
import s3j.ast.JsObject
import s3j.{*, given}

class MacroTest extends AnyFlatSpec with Matchers {
  it should "serialize simple classes" in {
    case class Test1(x: String, y: String) derives JsonFormat
    Test1("123", "qwe").toJsonString shouldBe "{\"x\":\"123\",\"y\":\"qwe\"}"
    "{\"x\":\"xxx\",\"y\":\"yyy\"}".convertTo[Test1] shouldBe Test1("xxx", "yyy")
    "{\"y\":\"YYY\",\"x\":\"XXX\"}".convertTo[Test1] shouldBe Test1("XXX", "YYY")
  }

  it should "serialize rest fields" in {
    case class Test2(x: String, @restFields y: JsObject) derives JsonFormat
    Test2("123", JsObject("f1" -> "123", "f2" -> 123)).toJsonString shouldBe "{\"x\":\"123\",\"f1\":\"123\",\"f2\":123}"

    val r = "{\"f1\":123,\"x\":\"qqq\",\"f2\":456,\"f3\": true}".convertTo[Test2]
    r shouldBe Test2("qqq", JsObject("f1" -> 123, "f2" -> 456, "f3" -> true))
    r.y.order shouldBe Seq("f1", "f2", "f3")
  }

  it should "serialize Option's" in {
    case class Test3(x: String, y: Option[String]) derives JsonFormat

    Test3("123", None).toJsonString shouldBe "{\"x\":\"123\"}"
    Test3("123", Some("456")).toJsonString shouldBe "{\"x\":\"123\",\"y\":\"456\"}"

    "{\"x\":\"123\"}".convertTo[Test3] shouldBe Test3("123", None)
    "{\"x\":\"123\",\"y\":null}".convertTo[Test3] shouldBe Test3("123", None)
    "{\"y\":\"qqq\",\"x\":\"123\"}".convertTo[Test3] shouldBe Test3("123", Some("qqq"))
  }

  it should "serialize Option's with @nullOption" in {
    case class Test4(x: String, @nullOption y: Option[String]) derives JsonFormat

    Test4("123", None).toJsonString shouldBe "{\"x\":\"123\",\"y\":null}"
    Test4("123", Some("456")).toJsonString shouldBe "{\"x\":\"123\",\"y\":\"456\"}"
  }

  it should "serialize nested case classes" in {
    case class Test5A(x: String)
    case class Test5(a: Test5A, b: Test5A) derives JsonFormat

    Test5(Test5A("123"), Test5A("qwe")).toJsonString shouldBe "{\"a\":{\"x\":\"123\"},\"b\":{\"x\":\"qwe\"}}"
    "{\"b\":{\"x\":\"a\"},\"a\":{\"x\":\"b\"}}".convertTo[Test5] shouldBe Test5(Test5A("b"), Test5A("a"))
  }

  it should "serialize generic nested case classes" in {
    case class Test6A[T](x: T)
    case class Test6(a: Test6A[String], b: Test6A[Test6A[String]]) derives JsonFormat

    Test6(Test6A("qwe"), Test6A(Test6A("asd"))).toJsonString shouldBe
      "{\"a\":{\"x\":\"qwe\"},\"b\":{\"x\":{\"x\":\"asd\"}}}"

    "{\"a\":{\"x\":\"qwe\"},\"b\":{\"x\":{\"x\":\"asd\"}}}".convertTo[Test6] shouldBe
      Test6(Test6A("qwe"), Test6A(Test6A("asd")))
  }
}
