package s3j.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.StringReader

class ReferenceReaderTest extends AnyFlatSpec with Matchers {
  private var index = 1
  private def test(json: String): Unit = {
    it should s"match reference ($index)" in {
      val contextLength = 32
      val tokens1 = new ReferenceJsonReader(json, contextLength).result
      val tokens2 = ReferenceJsonReader.gatherTokens(new StreamJsonReader(new StringReader(json),
        StreamJsonReader.defaultSettings.copy(contextLength = contextLength)))

      if (tokens1 != tokens2) {
        for (i <- 0 until math.max(tokens1.size, tokens2.size)) {
          val t1 = tokens1.lift(i)
          val t2 = tokens2.lift(i)

          if (t1 != t2) {
            println(s"Mismatch: i=$i")
            println("REF: " + t1.fold("<empty>")(_.toString()))
            println("GOT: " + t2.fold("<empty>")(_.toString()))
            println("---")
          }
        }
      }

      tokens1 shouldBe tokens2
    }

    index += 1
  }

  test("{}")
  test("[]")
  test("1234")
  test("\"meow\"")
  test("true")
  test("false")
  test("null")

  test("{\"qwe\":1}")
  test("{\"qwe\":[1,2,3,4]}")
  test("[{\"qwe\":\"asd\"}]")
  test("[{},{\"x\":[{},{\"x\":[1,2,3]}]},{},{}]")
  test("[{\"x\":1,\"y\":[1,2,3],\"z\":\"qwe\",\"w\":true}]")
  test("[[[[[[]]]]]]")
  test("[1,[1,[1,[1,1],1],1],1]")
  test("{\"x\":{\"x\":{\"x\":{\"x\":1,\"y\":2},\"y\":2},\"y\":2},\"y\":2}")

  test("{\n}\n")
  test("{\n\"x\":true\n}\n")
  test("{\n\t\"x\":123,\n\t\"y\":true,\n\t\"z\":\"qweqwe\"\n}\n")

  // at least 2x continuation tokens
  test("{\"x\":\"" + "loooongstring".repeat(2048) + "\"}")
}
