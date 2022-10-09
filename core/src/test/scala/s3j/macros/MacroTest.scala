package s3j.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.annotations.*
import s3j.annotations.naming.{capitalizedKebabCase, screamingSnakeCase, snakeCase}
import s3j.ast.JsObject
import s3j.io.ParseException
import s3j.{*, given}

class MacroTest extends AnyFlatSpec with Matchers {
  it should "serialize simple classes" in {
    case class Test(x: String, y: String) derives JsonFormat
    Test("123", "qwe").toJsonString shouldBe "{\"x\":\"123\",\"y\":\"qwe\"}"
    "{\"x\":\"xxx\",\"y\":\"yyy\"}".convertTo[Test] shouldBe Test("xxx", "yyy")
    "{\"y\":\"YYY\",\"x\":\"XXX\"}".convertTo[Test] shouldBe Test("XXX", "YYY")

    an [ParseException] shouldBe thrownBy { "{}".convertTo[Test] }
    an [ParseException] shouldBe thrownBy { "{\"x\":\"\",\"y\":\"\",\"z\":\"123\"}".convertTo[Test] }
  }

  it should "serialize rest fields" in {
    case class Test(x: String, @restFields y: JsObject) derives JsonFormat
    Test("123", JsObject("f1" -> "123", "f2" -> 123)).toJsonString shouldBe "{\"x\":\"123\",\"f1\":\"123\",\"f2\":123}"

    val r = "{\"f1\":123,\"x\":\"qqq\",\"f2\":456,\"f3\": true}".convertTo[Test]
    r shouldBe Test("qqq", JsObject("f1" -> 123, "f2" -> 456, "f3" -> true))
    r.y.order shouldBe Seq("f1", "f2", "f3")
  }

  it should "handle @allowUnknownKeys" in {
    @allowUnknownKeys
    case class Test(x: String) derives JsonFormat

    "{\"x\":\"123\",\"y\":\"123123\",\"z\":\"123\",\"w\":{\"x\":123,\"y\":true}}".convertTo[Test] shouldBe Test("123")
  }

  it should "serialize Option's" in {
    case class Test(x: String, y: Option[String]) derives JsonFormat

    Test("123", None).toJsonString shouldBe "{\"x\":\"123\"}"
    Test("123", Some("456")).toJsonString shouldBe "{\"x\":\"123\",\"y\":\"456\"}"

    "{\"x\":\"123\"}".convertTo[Test] shouldBe Test("123", None)
    "{\"x\":\"123\",\"y\":null}".convertTo[Test] shouldBe Test("123", None)
    "{\"y\":\"qqq\",\"x\":\"123\"}".convertTo[Test] shouldBe Test("123", Some("qqq"))
  }

  it should "serialize Option's with @nullOption" in {
    case class Test(x: String, @nullOption y: Option[String]) derives JsonFormat

    Test("123", None).toJsonString shouldBe "{\"x\":\"123\",\"y\":null}"
    Test("123", Some("456")).toJsonString shouldBe "{\"x\":\"123\",\"y\":\"456\"}"
  }

  it should "serialize nested case classes" in {
    case class TestA(x: String)
    case class Test(a: TestA, b: TestA) derives JsonFormat

    Test(TestA("123"), TestA("qwe")).toJsonString shouldBe "{\"a\":{\"x\":\"123\"},\"b\":{\"x\":\"qwe\"}}"
    "{\"b\":{\"x\":\"a\"},\"a\":{\"x\":\"b\"}}".convertTo[Test] shouldBe Test(TestA("b"), TestA("a"))
  }

  it should "serialize generic nested case classes" in {
    case class TestA[T](x: T)
    case class Test(a: TestA[String], b: TestA[TestA[String]]) derives JsonFormat

    Test(TestA("qwe"), TestA(TestA("asd"))).toJsonString shouldBe
      "{\"a\":{\"x\":\"qwe\"},\"b\":{\"x\":{\"x\":\"asd\"}}}"

    "{\"a\":{\"x\":\"qwe\"},\"b\":{\"x\":{\"x\":\"asd\"}}}".convertTo[Test] shouldBe
      Test(TestA("qwe"), TestA(TestA("asd")))
  }

  it should "recognize annotations for object keys" in {
    case class Test(@key("mew") x: String, @snakeCase meowOink: String, @capitalizedKebabCase barkHonk: String)
    derives JsonFormat

    Test("123", "456", "789").toJsonString shouldBe "{\"mew\":\"123\",\"meow_oink\":\"456\",\"Bark-Honk\":\"789\"}"
    "{\"mew\":\"XXX\",\"meow_oink\":\"YYY\",\"Bark-Honk\":\"ZZZ\"}".convertTo[Test] shouldBe Test("XXX", "YYY", "ZZZ")
  }

  it should "recognize global annotations for case convention" in {
    @screamingSnakeCase
    object TestW {
      case class TestA(someKey: String)
      case class Test(meowOink: String, barkHonk: TestA) derives JsonFormat
    }

    import TestW.*

    Test("123", TestA("xyz")).toJsonString shouldBe "{\"MEOW_OINK\":\"123\",\"BARK_HONK\":{\"SOME_KEY\":\"xyz\"}}"
    "{\"MEOW_OINK\":\"A\",\"BARK_HONK\":{\"SOME_KEY\":\"B\"}}".convertTo[Test] shouldBe Test("A", TestA("B"))
  }

  it should "serialize stringy enums" in {
    enum Test derives JsonFormat {
      case Meow
      case Bark
    }

    Test.Meow.toJsonString shouldBe "\"Meow\""
    "\"Bark\"".convertTo[Test] shouldBe Test.Bark

    an [ParseException] shouldBe thrownBy {
      "\"Oink\"".convertTo[Test]
    }
  }

  it should "recognize discriminator annotations for stringy enums" in {
    @screamingSnakeCase
    enum Test derives JsonFormat {
      case MeowOink

      @discriminator("bark!honk")
      case BarkHonk

      @capitalizedKebabCase
      case QuackQuack
    }

    Test.MeowOink.toJsonString shouldBe "\"MEOW_OINK\""
    Test.BarkHonk.toJsonString shouldBe "\"bark!honk\""
    Test.QuackQuack.toJsonString shouldBe "\"Quack-Quack\""

    "\"MEOW_OINK\"".convertTo[Test] shouldBe Test.MeowOink
    "\"bark!honk\"".convertTo[Test] shouldBe Test.BarkHonk
    "\"Quack-Quack\"".convertTo[Test] shouldBe Test.QuackQuack
  }

  it should "serialize parametrized enums" in {
    enum Test derives JsonFormat {
      case A(x: String)
      case B(y: String, z: String)
      case C
    }

    Test.A("123").toJsonString shouldBe "{\"type\":\"A\",\"x\":\"123\"}"
    Test.B("x", "y").toJsonString shouldBe "{\"type\":\"B\",\"y\":\"x\",\"z\":\"y\"}"
    Test.C.toJsonString shouldBe "{\"type\":\"C\"}"

    "{\"type\":\"A\",\"x\":\"xx\"}".convertTo[Test] shouldBe Test.A("xx")
    "{\"type\":\"B\",\"y\":\"1\",\"z\":\"2\"}".convertTo[Test] shouldBe Test.B("1", "2")
    "{\"type\":\"C\"}".convertTo[Test] shouldBe Test.C
  }

  it should "fail on out-of-order discriminator if no @allowBuffering is set" in {
    enum Test derives JsonFormat {
      case A(x: String)
      case B
    }

    "{\"type\":\"A\",\"x\":\"1\"}".convertTo[Test] shouldBe Test.A("1")

    a [ParseException] shouldBe thrownBy {
      "{\"x\":\"1\",\"type\":\"A\"}".convertTo[Test]
    }
  }

  it should "work with out-of-order discriminator if @allowBuffering is set" in {
    @allowBuffering
    enum Test derives JsonFormat {
      case A(x: String)
      case B
    }

    "{\"x\":\"1\",\"type\":\"A\"}".convertTo[Test] shouldBe Test.A("1")
  }

  it should "fail on extra fields in singleton enum cases" in {
    enum Test derives JsonFormat {
      case A
      case B(x: String) // to force objects
    }

    "{\"type\":\"A\"}".convertTo[Test] shouldBe Test.A

    a [ParseException] shouldBe thrownBy {
      "{\"type\":\"A\",\"x\":\"1\"}".convertTo[Test]
    }
  }

  it should "allow extra fields in singleton enum cases with @allowUnknownKeys" in {
    @allowUnknownKeys
    enum Test derives JsonFormat {
      case A
      case B(x: String) // to force objects
    }

    "{\"type\":\"A\"}".convertTo[Test] shouldBe Test.A
    "{\"type\":\"A\",\"x\":\"1\"}".convertTo[Test]
  }

  it should "respect @stringyCases annotation" in {
    @stringyCases
    enum Test derives JsonFormat {
      case A
      case B(x: String)
    }

    Test.A.toJsonString shouldBe "\"A\""
  }

  it should "respect @objectEnum annotation" in {
    @objectEnum
    enum Test derives JsonFormat {
      case A
    }

    Test.A.toJsonString shouldBe "{\"type\":\"A\"}"
  }

  it should "respect @discriminator and @discriminatorField annotation" in {
    @discriminatorField("mew")
    enum Test derives JsonFormat {
      case A(x: String)

      @discriminator("X")
      case B
    }

    Test.A("123").toJsonString shouldBe "{\"mew\":\"A\",\"x\":\"123\"}"
    "{\"mew\":\"A\",\"x\":\"X\"}".convertTo[Test] shouldBe Test.A("X")

    Test.B.toJsonString shouldBe "{\"mew\":\"X\"}"
    "{\"mew\":\"X\"}".convertTo[Test] shouldBe Test.B
  }

  it should "generate generic sealed hierarchies" in {
    sealed trait Test derives JsonFormat
    case class TestA(x: String) extends Test
    case class TestB(y: String) extends Test

    TestA("1").toJsonString shouldBe "{\"type\":\"TestA\",\"x\":\"1\"}"
    TestB("2").toJsonString shouldBe "{\"type\":\"TestB\",\"y\":\"2\"}"

    "{\"type\":\"TestA\",\"x\":\"X\"}".convertTo[Test] shouldBe TestA("X")
    "{\"type\":\"TestB\",\"y\":\"Y\"}".convertTo[Test] shouldBe TestB("Y")
  }
}
