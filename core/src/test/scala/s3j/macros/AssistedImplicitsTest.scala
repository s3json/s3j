package s3j.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.{*, given}

object AssistedImplicitsTest {
  case class Payload(x: Int)
  object Payload {
    given JsonFormat[Payload] =
      summon[JsonFormat[String]].mapFormat(p => s"custom:${p.x}", s => Payload(s.stripPrefix("custom:").toInt))
  }

  // `JsonFormat[Payload]` is required by `Holder` format, but `Payload` companion is not a part of `JsonFormat[Holder]`
  case class Holder(payload: Payload)
  object Holder {
    given holderFormat(using p: JsonFormat[Payload]): JsonFormat[Holder] = p.mapFormat(_.payload, Holder(_))
  }

  case class Outer(h: Holder) derives JsonFormat
}

class AssistedImplicitsTest extends AnyFlatSpec with Matchers {
  import AssistedImplicitsTest.*

  it should "prefer companion instances over generated ones for nested requirements" in {
    Outer(Holder(Payload(1))).toJsonString shouldBe "{\"h\":\"custom:1\"}"
    "{\"h\":\"custom:2\"}".fromJson[Outer] shouldBe Outer(Holder(Payload(2)))
  }
}
