package s3j.macros.generic

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.macros.generic.GenerationMode

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
        if (base =:= TypeRepr.of[JsonDecoder]) (Decoder, arg)
        else if (base =:= TypeRepr.of[JsonEncoder]) (Encoder, arg)
        else if (base =:= TypeRepr.of[JsonFormat]) (Format, arg)
        else throwError

      case _ => throwError
    }
  }

  /** Decoder-only generation mode */
  case object Decoder extends GenerationMode {
    def generateEncoders: Boolean = false
    def generateDecoders: Boolean = true

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonDecoder].appliedTo(t)
    }
  }

  /** Encoder-only generation mode */
  case object Encoder extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = false

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonEncoder].appliedTo(t)
    }
  }

  /** Decoder-and-encoder (format) generation mode */
  case object Format extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = true

    def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonFormat].appliedTo(t)
    }
  }
}

sealed trait GenerationMode {
  /** @return Whether we should generate encoders in this mode or not */
  def generateEncoders: Boolean

  /** @return Whether we should generate decoders in this mode or not */
  def generateDecoders: Boolean

  /** @return Type class applied to given type (e.g. `JsonDecoder[X]` when decoding) */
  def appliedType(using q: Quotes)(t: q.reflect.TypeRepr): q.reflect.TypeRepr
}
