package s3j.macros.traits

import s3j.format.{JsonDecoder, JsonEncoder}

import java.lang.module.ModuleDescriptor.Exports
import scala.quoted.Expr

trait NestedResult[T] {
  /** Raw generation result */
  def raw: Expr[Any]
  
  /** Raw generation result, typed as [[JsonEncoder]] */
  def encoder: Expr[JsonEncoder[T]]
  
  /** Raw generation result, typed as [[JsonDecoder]] */
  def decoder: Expr[JsonDecoder[T]]
}
