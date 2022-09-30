import s3j.*
import s3j.annotations.{allowUnknownKeys, requireImplicit, restFields}
import s3j.ast.{JsInt, JsObject, JsString}
import s3j.io.{JsonReader, JsonWriter, StreamJsonWriter}

import java.io.StringWriter

object MeowingTest extends App {
  import format.BasicFormats.stringFormat
  import io.IoExtensions.*
  

  case class Test(x: String, @meowingString y: String, @restFields rest: JsObject) derives JsonFormat

  val t = Test("123", "MEOW", JsObject("zzz" -> JsInt(123)))
  val sw = new StringWriter()
  val jw = new StreamJsonWriter(sw, indent = 2)
  implicitly[JsonFormat[Test]].encode(jw, t)
  println(sw.toString)

  val s = "{\"x\":\"qwe\",\"y\":\"666\",\"sss\":[123,true]}"
  println(s.convertTo[Test])
}
