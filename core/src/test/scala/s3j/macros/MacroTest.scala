package s3j.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.annotations.{nullOption, restFields}
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
}
