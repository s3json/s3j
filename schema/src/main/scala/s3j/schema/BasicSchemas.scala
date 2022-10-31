package s3j.schema

import java.util.UUID

object BasicSchemas {
  given booleanSchema: InlineSchema[Boolean, "{\"type\":\"boolean\"}", Unit] =
    InlineSchema.make

  given stringSchema: InlineSchema[String, "{\"type\":\"string\"}", Unit] =
    InlineSchema.make

  given uuidSchema: InlineSchema[UUID, "{\"type\":\"string\",\"format\":\"uuid\"}", Unit] =
    InlineSchema.make

  given byteSchema: InlineSchema[Byte, "{\"type\":\"integer\",\"format\":\"int8\"}", Unit] =
    InlineSchema.make

  given shortSchema: InlineSchema[Short, "{\"type\":\"integer\",\"format\":\"int16\"}", Unit] =
    InlineSchema.make

  given intSchema: InlineSchema[Int, "{\"type\":\"integer\",\"format\":\"int32\"}", Unit] =
    InlineSchema.make

  given longSchema: InlineSchema[Long, "{\"type\":\"integer\",\"format\":\"int64\"}", Unit] =
    InlineSchema.make

  given floatSchema: InlineSchema[Float, "{\"type\":\"number\"}", Unit] =
    InlineSchema.make

  given doubleSchema: InlineSchema[Double, "{\"type\":\"double\"}", Unit] =
    InlineSchema.make
}
