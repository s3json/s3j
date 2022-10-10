package s3j.format
import s3j.io.JsonWriter

trait StringyEncoder[T] { outer =>
  /** @return Encoded representation of [[value]] */
  def encode(value: T): String
  
  /** JSON decoder for decoding strings */
  lazy val json: JsonEncoder[T] =
    new JsonEncoder[T] {
      def encode(writer: JsonWriter, value: T): Unit = writer.stringValue(outer.encode(value))
      override def toString: String = s"$outer.json"
    }

  /** @return [[JsonEncoder]] which applies a function to the encoded value */
  def mapEncoded[R](f: R => T): StringyEncoder[R] =
    new StringyEncoder[R] {
      def encode(value: R): String = outer.encode(f(value))
      override def toString: String = s"$outer.mapEncoded($f)"
    }
}
