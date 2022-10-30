package s3j

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.internal.InternalMacros

import scala.annotation.unused

extension (@unused x: JsonDecoder.type) {
  inline def derived[T]: JsonDecoder[T] = InternalMacros.derived[JsonDecoder[T]]
}

extension (@unused x: JsonEncoder.type) {
  inline def derived[T]: JsonEncoder[T] = InternalMacros.derived[JsonEncoder[T]]
}

extension (@unused x: JsonFormat.type) {
  inline def derived[T]: JsonFormat[T] = InternalMacros.derived[JsonFormat[T]]
}
