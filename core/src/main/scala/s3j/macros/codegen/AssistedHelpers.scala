package s3j.macros.codegen

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, StringyDecoder, StringyEncoder, StringyFormat}
import s3j.schema.JsonSchema

import scala.annotation.compileTimeOnly

/**
 * Helpers for assisted implicit search. They are different for each generation mode to prevent compiler from picking
 * completely wrong overload which we cannot generate.
 */
object AssistedHelpers {
  object Decoder {
    @compileTimeOnly("assistedSearch placeholder")
    given $assistedDecoder[T]: JsonDecoder[T] = compileTime

    @compileTimeOnly("assistedSearch placeholder")
    given $assistedStringyDecoder[T]: StringyDecoder[T] = compileTime
  }

  object Encoder {
    @compileTimeOnly("assistedSearch placeholder")
    given $assistedEncoder[T]: JsonEncoder[T] = compileTime

    @compileTimeOnly("assistedSearch placeholder")
    given $assistedStringyEncoder[T]: StringyEncoder[T] = compileTime
  }

  object Format {
    @compileTimeOnly("assistedSearch placeholder")
    given $assistedFormat[T]: JsonFormat[T] = compileTime

    @compileTimeOnly("assistedSearch placeholder")
    given $assistedStringyFormat[T]: StringyFormat[T] = compileTime
  }

  object Schema {
    @compileTimeOnly("assistedSearch placeholder")
    given $assistedSchema[T]: JsonSchema[T] = compileTime
  }
  
  private def compileTime: Nothing = throw new RuntimeException("Compile time only")
}
