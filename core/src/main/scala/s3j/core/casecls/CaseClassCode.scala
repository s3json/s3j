package s3j.core.casecls

import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.generic.GenerationMode

import scala.quoted.{Expr, Quotes}

trait CaseClassCode[T] {
  /** @return Encoder code block */
  def encoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit]

  /** @return Decoder code block */
  def decoder(reader: Expr[JsonReader])(using Quotes): Expr[T]

  /** @return Complete format (wrapped with class corresponding to generation mode) */
  def format(mode: GenerationMode)(using Quotes): Expr[Any]
}
