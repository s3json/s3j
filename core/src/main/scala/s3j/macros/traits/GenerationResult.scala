package s3j.macros.traits

import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.schema.SchemaExpr

import java.lang.module.ModuleDescriptor.Exports
import scala.quoted.{Expr, Quotes}

trait GenerationResult[T] {
  /** Raw generation result. For schemas, this is `schema.asExpr` */
  def raw(using Quotes): Expr[Any]
  
  /** Raw generation result, typed as [[JsonEncoder]] */
  def encoder(using Quotes): Expr[JsonEncoder[T]]

  /** @return Encoding expression, which could be more efficient than calling [[JsonEncoder]] object */
  def encode(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit]
  
  /** Raw generation result, typed as [[JsonDecoder]] */
  def decoder(using Quotes): Expr[JsonDecoder[T]]

  /** @return Decoding expression, which could be more efficient than calling [[JsonDecoder]] object */
  def decode(reader: Expr[JsonReader])(using Quotes): Expr[T]

  /** JSON schema expression */
  def schema: SchemaExpr[T]
}
