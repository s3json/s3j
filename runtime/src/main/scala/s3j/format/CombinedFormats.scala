package s3j.format

import s3j.io.{JsonReader, JsonWriter}

object CombinedFormats {
  trait StringyJsonEncoder[T] extends JsonEncoder[T] with StringyEncoder[T] { outer =>
    override lazy val json: JsonEncoder[T] = this

    override def mapEncoded[R](f: R => T): StringyJsonEncoder[R] =
      new StringyJsonEncoder[R] {
        def encode(writer: JsonWriter, value: R): Unit = outer.encode(writer, f(value))
        def encode(value: R): String = outer.encode(f(value))
      }
  }

  trait StringyJsonDecoder[T] extends JsonDecoder[T] with StringyDecoder[T] { outer =>
    override lazy val json: JsonDecoder[T] = this

    override def mapDecoded[R](f: T => R): StringyJsonDecoder[R] =
      new StringyJsonDecoder[R] {
        def decode(reader: JsonReader): R = f(outer.decode(reader))
        def decode(str: String): R = f(outer.decode(str))
        override def maxLength: Int = outer.maxLength
      }
  }

  trait StringyJsonFormat[T] extends StringyJsonDecoder[T] with StringyJsonEncoder[T] 
  with JsonFormat[T] with StringyFormat[T] { outer =>
    override lazy val json: JsonFormat[T] = this

    override def mapFormat[R](encoding: R => T, decoding: T => R): StringyJsonFormat[R] =
      new StringyJsonFormat[R] {
        def encode(writer: JsonWriter, value: R): Unit = outer.encode(writer, encoding(value))
        def decode(reader: JsonReader): R = decoding(outer.decode(reader))
        def encode(value: R): String = outer.encode(encoding(value))
        def decode(str: String): R = decoding(outer.decode(str))
        override def maxLength: Int = outer.maxLength
      }
  }
}
