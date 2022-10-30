package s3j.internal.utils

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}

import scala.quoted.Quotes

object GenerationMode {
  /** Decode generation mode from applied type. E.g. `decode(JsonFormat[Something])` returns `(Format, Something)`. */
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

  /** Encoding generation mode */
  case object Encoder extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = false
    def appliedType(using q: Quotes)(tpe: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonEncoder].appliedTo(tpe)
    }
  }

  /** Decoding generation mode */
  case object Decoder extends GenerationMode {
    def generateEncoders: Boolean = false
    def generateDecoders: Boolean = true
    def appliedType(using q: Quotes)(tpe: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonDecoder].appliedTo(tpe)
    }
  }

  /** Format generation mode */
  case object Format extends GenerationMode {
    def generateEncoders: Boolean = true
    def generateDecoders: Boolean = true

    def appliedType(using q: Quotes)(tpe: q.reflect.TypeRepr): q.reflect.TypeRepr = {
      import q.reflect.*
      TypeRepr.of[JsonFormat].appliedTo(tpe)
    }
  }
}

sealed trait GenerationMode {
  /** @return Whether we should generate encoders */
  def generateEncoders: Boolean

  /** @return Whether we should generate decoders */
  def generateDecoders: Boolean

  /** @return Applied type for this mode (i.e. `T` -> `JsonSomething[T]`) */
  def appliedType(using q: Quotes)(tpe: q.reflect.TypeRepr): q.reflect.TypeRepr
}
