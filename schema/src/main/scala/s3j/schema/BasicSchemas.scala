package s3j.schema

import java.util.UUID

object BasicSchemas {
  given booleanSchema: InlineSchema[Boolean, "{\"type\":\"boolean\"}", Unit] =
    InlineSchema.make

  given stringSchema: InlineSchema[String, "{\"type\":\"string\"}", Unit] =
    InlineSchema.make

  given uuidSchema: InlineSchema[UUID, "{\"type\":\"string\",\"format\":\"uuid\"}", Unit] =
    InlineSchema.make

  given byteSchema: InlineSchema[Byte, "{\"type\":\"integer\",\"format\":\"uint8\"}", Unit] =
    InlineSchema.make

  given shortSchema: InlineSchema[Short, "{\"type\":\"integer\",\"format\":\"uint16\"}", Unit] =
    InlineSchema.make

  given intSchema: InlineSchema[Int, "{\"type\":\"integer\",\"format\":\"uint32\"}", Unit] =
    InlineSchema.make

  given longSchema: InlineSchema[Long, "{\"type\":\"integer\",\"format\":\"uint64\"}", Unit] =
    InlineSchema.make

  given floatSchema: InlineSchema[Float, "{\"type\":\"number\"}", Unit] =
    InlineSchema.make

  given doubleSchema: InlineSchema[Double, "{\"type\":\"double\"}", Unit] =
    InlineSchema.make
}
