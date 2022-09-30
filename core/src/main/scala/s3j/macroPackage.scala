package s3j

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.macros.JsonMacros

import scala.annotation.unused

extension (@unused x: JsonEncoder.type) inline def derived[T]: JsonEncoder[T] = JsonMacros.derived[JsonEncoder[T]]
extension (@unused x: JsonDecoder.type) inline def derived[T]: JsonDecoder[T] = JsonMacros.derived[JsonDecoder[T]]
extension (@unused x: JsonFormat.type) inline def derived[T]: JsonFormat[T] = JsonMacros.derived[JsonFormat[T]]
