package s3j.macros.traits

import s3j.format.{JsonDecoder, JsonEncoder}

import java.lang.module.ModuleDescriptor.Exports
import scala.quoted.{Expr, Quotes}

trait NestedResult[T] {
  /** Raw generation result */
  def raw(using Quotes): Expr[Any]
  
  /** Raw generation result, typed as [[JsonEncoder]] */
  def encoder(using Quotes): Expr[JsonEncoder[T]]
  
  /** Raw generation result, typed as [[JsonDecoder]] */
  def decoder(using Quotes): Expr[JsonDecoder[T]]
}
