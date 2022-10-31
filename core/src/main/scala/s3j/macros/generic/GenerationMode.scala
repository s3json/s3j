package s3j.macros.generic

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, StringyDecoder, StringyEncoder, StringyFormat}
import s3j.macros.generic.GenerationMode
import s3j.schema.JsonSchema

import scala.compiletime.error
import scala.quoted.*

object GenerationMode {
  /**
   * Decode generation mode from applied type. E.g. `decode(JsonFormat[Something])` returns `(Format, Something)`.
   *
   * @param t Applied type to decode
   * @return  Decoded generation mode and actual type to be generated
   */
  def decode(using q: Quotes)(t: q.reflect.TypeRepr): (GenerationMode, q.reflect.TypeRepr) = {
    import q.reflect.*

    def throwError: Nothing =
      report.errorAndAbort(s"Failed to determine generation mode from type ${t.show}. " +
        s"Expected either JsonDecoder[?], JsonEncoder[?] or JsonFormat[?]")

    t match {
      case AppliedType(base, List(arg)) =>
        val mode = base.typeSymbol.fullName match {
          case "s3j.format.JsonDecoder"     => GenerationMode.Decoder
          case "s3j.format.JsonEncoder"     => GenerationMode.Encoder
          case "s3j.format.JsonFormat"      => GenerationMode.Format
          case "s3j.format.StringyDecoder"  => GenerationMode.StringDecoder
          case "s3j.format.StringyEncoder"  => GenerationMode.StringEncoder
          case "s3j.format.StringyFormat"   => GenerationMode.StringFormat
          case "s3j.schema.JsonSchema"      => GenerationMode.Schema
          case _ => throwError
        }

        mode -> arg

      case _ => throwError
    }
  }

  /** Decoder-only generation mode */
  case object Decoder extends GenerationMode {
    def generateEncoders: Boolean = false
    def generateDecoders: Boolean = true
    def stringy: Boolean = false

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonDecoder].appliedTo(t)
    }
  }

  case object StringDecoder extends GenerationMode {
    def generateEncoders: Boolean = false
    def generateDecoders: Boolean = true
    def stringy: Boolean = true

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[StringyDecoder].appliedTo(t)
    }
  }

  /** Encoder-only generation mode */
  case object Encoder extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = false
    def stringy: Boolean = false

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonEncoder].appliedTo(t)
    }
  }

  case object StringEncoder extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = false
    def stringy: Boolean = true

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[StringyEncoder].appliedTo(t)
    }
  }

  /** Decoder-and-encoder (format) generation mode */
  case object Format extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = true
    def stringy: Boolean = false

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonFormat].appliedTo(t)
    }
  }

  /** Decoder-and-encoder (format) generation mode */
  case object StringFormat extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = true
    def stringy: Boolean = true

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[StringyFormat].appliedTo(t)
    }
  }

  /** Schema generation mode */
  case object Schema extends GenerationMode {
    def generateEncoders: Boolean = false
    def generateDecoders: Boolean = false
    def stringy: Boolean = false

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonSchema].appliedTo(t)
    }
  }
}

sealed trait GenerationMode {
  /** @return Whether we should generate encoders in this mode or not */
  def generateEncoders: Boolean

  /** @return Whether we should generate decoders in this mode or not */
  def generateDecoders: Boolean

  /** @return Whether this mode means stringy result */
  def stringy: Boolean

  /** @return Whether this mode means generation of schema */
  def schema: Boolean = this == GenerationMode.Schema

  /** @return Whether two modes are compatible in terms of decoding/encoding/formatting */
  def modeCompatible(other: GenerationMode): Boolean =
    schema == other.schema && generateEncoders == other.generateEncoders && generateDecoders == other.generateDecoders

  /** @return Type class applied to given type (e.g. `JsonDecoder[X]` when decoding) */
  def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr
}
