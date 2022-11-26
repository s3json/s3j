package s3j.core.primitives

import s3j.core.primitives.PrimitiveSpecification.{EncodeFunc, FormatFunc, FormatSpecification}
import s3j.format.impl.NumberFormats
import s3j.format.util.DecoderUtils
import s3j.format.{BasicFormats, JsonFormat}
import s3j.io.{JsonReader, JsonWriter}
import s3j.schema.{BasicSchemas, JsonSchema}

import scala.quoted.{Expr, Quotes, Type}

object PrimitiveSpecification {
  type EncodeFunc[T] = Quotes ?=> (Expr[JsonWriter], Expr[T]) => Expr[Unit]
  type DecodeFunc[T] = Quotes ?=> Expr[JsonReader] => Expr[T]
  type FormatFunc[T] = Quotes ?=> Expr[JsonFormat[T]]
  type SchemaFunc[T] = Quotes ?=> Expr[JsonSchema[T]]

  class FormatSpecification[T](
    val encode: EncodeFunc[T],
    val decode: DecodeFunc[T],
    val format: FormatFunc[T],
    val schema: SchemaFunc[T]
  )

  val Boolean: PrimitiveSpecification[Boolean] = PrimitiveSpecification(
    typeName = "scala.Boolean",
    default = new FormatSpecification[Boolean](
      encode = (w, v) => '{ $w.boolValue($v) },
      decode = r => '{ $r.readBoolean() },
      format = '{ BasicFormats.booleanFormat },
      schema = '{ BasicSchemas.booleanSchema }
    ),
    unsigned = None
  )

  val Byte: PrimitiveSpecification[Byte] = PrimitiveSpecification(
    typeName = "scala.Byte",
    default = new FormatSpecification[Byte](
      encode = (w, v) => '{ $w.byteValue($v) },
      decode = r => '{ $r.readByte() },
      format = '{ NumberFormats.byteFormat },
      schema = '{ BasicSchemas.byteSchema }
    ),
    unsigned = Some(new FormatSpecification[Byte](
      encode = (w, v) => '{ $w.unsignedByteValue($v) },
      decode = r => '{ $r.readUnsignedByte() },
      format = '{ NumberFormats.unsignedByteFormat },
      schema = '{ BasicSchemas.unsignedByteSchema }
    ))
  )

  val Short: PrimitiveSpecification[Short] = PrimitiveSpecification(
    typeName = "scala.Short",
    default = new FormatSpecification[Short](
      encode = (w, v) => '{ $w.shortValue($v) },
      decode = r => '{ $r.readShort() },
      format = '{ NumberFormats.shortFormat },
      schema = '{ BasicSchemas.shortSchema }
    ),
    unsigned = Some(new FormatSpecification[Short](
      encode = (w, v) => '{ $w.unsignedShortValue($v) },
      decode = r => '{ $r.readUnsignedShort() },
      format = '{ NumberFormats.unsignedShortFormat },
      schema = '{ BasicSchemas.unsignedShortSchema }
    ))
  )

  val Int: PrimitiveSpecification[Int] = PrimitiveSpecification(
    typeName = "scala.Int",
    default = new FormatSpecification[Int](
      encode = (w, v) => '{ $w.intValue($v) },
      decode = r => '{ $r.readInt() },
      format = '{ NumberFormats.intFormat },
      schema = '{ BasicSchemas.intSchema }
    ),
    unsigned = Some(new FormatSpecification[Int](
      encode = (w, v) => '{ $w.unsignedIntValue($v) },
      decode = r => '{ $r.readUnsignedInt() },
      format = '{ NumberFormats.unsignedIntFormat },
      schema = '{ BasicSchemas.unsignedIntSchema }
    ))
  )
  
  val Long: PrimitiveSpecification[Long] = PrimitiveSpecification(
    typeName = "scala.Long",
    default = new FormatSpecification[Long](
      encode = (w, v) => '{ $w.longValue($v) },
      decode = r => '{ $r.readLong() },
      format = '{ NumberFormats.longFormat },
      schema = '{ BasicSchemas.longSchema }
    ),
    unsigned = Some(new FormatSpecification[Long](
      encode = (w, v) => '{ $w.unsignedLongValue($v) },
      decode = r => '{ $r.readUnsignedLong() },
      format = '{ NumberFormats.unsignedLongFormat },
      schema = '{ BasicSchemas.unsignedLongSchema }
    ))
  )
  
  val Float: PrimitiveSpecification[Float] = PrimitiveSpecification(
    typeName = "scala.Float",
    default = new FormatSpecification[Float](
      encode = (w, v) => '{ $w.floatValue($v) },
      decode = r => '{ $r.readFloat() },
      format = '{ NumberFormats.floatFormat },
      schema = '{ BasicSchemas.floatSchema }
    ),
    unsigned = None
  )
  
  val Double: PrimitiveSpecification[Double] = PrimitiveSpecification(
    typeName = "scala.Double",
    default = new FormatSpecification[Double](
      encode = (w, v) => '{ $w.doubleValue($v) },
      decode = r => '{ $r.readDouble() },
      format = '{ NumberFormats.doubleFormat },
      schema = '{ BasicSchemas.doubleSchema }
    ),
    unsigned = None
  )
  
  val String: PrimitiveSpecification[String] = PrimitiveSpecification(
    typeName = "java.lang.String",
    default = new FormatSpecification[String](
      encode = (w, v) => '{ $w.stringValue($v) },
      decode = r => '{ $r.readString() },
      format = '{ BasicFormats.stringFormat },
      schema = '{ BasicSchemas.stringSchema }
    ),
    unsigned = None
  )

  val All: Map[String, PrimitiveSpecification[?]] = Set(Boolean, Byte, Short, Int, Long, Float, Double, String)
    .map(s => s.typeName -> s).toMap
}

case class PrimitiveSpecification[T](
  typeName: String, // we can't get Type[T] without quotes
  default: FormatSpecification[T],
  unsigned: Option[FormatSpecification[T]]
)
