package s3j

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.schema.JsonSchema
import s3j.macros.JsonMacros

import scala.annotation.unused

extension (@unused x: JsonEncoder.type) inline def derived[T]: JsonEncoder[T] = JsonMacros.derived[JsonEncoder[T]]
extension (@unused x: JsonDecoder.type) inline def derived[T]: JsonDecoder[T] = JsonMacros.derived[JsonDecoder[T]]
extension (@unused x: JsonFormat.type) inline def derived[T]: JsonFormat[T] = JsonMacros.derived[JsonFormat[T]]
extension (@unused x: JsonSchema.type) inline def derived[T]: JsonSchema[T] = JsonMacros.derived[JsonSchema[T]]

inline given generatedEncoder[T]: JsonEncoder.Generated[T] =
  new JsonEncoder.Generated[T] { given codec: JsonEncoder[T] = JsonMacros.derived[JsonEncoder[T]] }

inline given generatedDecoder[T]: JsonDecoder.Generated[T] =
  new JsonDecoder.Generated[T] { given codec: JsonDecoder[T] = JsonMacros.derived[JsonDecoder[T]] }

inline given generatedFormat[T]: JsonFormat.Generated[T] =
  new JsonFormat.Generated[T] { given codec: JsonFormat[T] = JsonMacros.derived[JsonFormat[T]] }
