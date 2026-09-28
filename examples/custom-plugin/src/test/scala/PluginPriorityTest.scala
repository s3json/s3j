import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.{*, given}

object PluginPriorityTest {
  // Both MeowingPlugin (requested explicitly) and built-in primitives plugin are able to generate String format:
  case class Message(plain: String, @meowingString loud: String) derives JsonFormat
  case class TypeAnnotated(loud: String @meowingString) derives JsonFormat
}

class PluginPriorityTest extends AnyFlatSpec with Matchers {
  import PluginPriorityTest.*

  it should "prefer explicitly requested plugins over builtin primitives" in {
    val m = Message("a b", "c d")
    m.toJsonString shouldBe """{"plain":"a b","loud":"c meow d"}"""
    m.toJsonString.fromJson[Message] shouldBe m
  }

  it should "prefer explicitly requested plugins over builtin primitives for annotated types" in {
    TypeAnnotated("x y").toJsonString shouldBe """{"loud":"x meow y"}"""
  }
}
