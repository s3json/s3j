package s3j.core.enums

import s3j.macros.GenerationContext
import s3j.macros.GenerationContext.GenerationCandidate
import s3j.macros.modifiers.ModifierSet

import scala.quoted.{Expr, Quotes, Type}

private[enums] class ObjectCandidate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T])
extends GenerationCandidate {
  def confidence: Option[Int] = ???
  def identity: AnyRef = ???
  def generate(using Quotes)(): Expr[Any] = ???
}
