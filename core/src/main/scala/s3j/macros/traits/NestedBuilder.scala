package s3j.macros.traits

import s3j.macros.modifiers.ModifierSet
import s3j.macros.utils.GenerationPath

import scala.quoted.{Expr, Quotes}

/** Builder for generation of nested serializers. */
trait NestedBuilder[T] {
  /** Discard modifiers inferred from type itself, leaving empty set of modifiers */
  def discardModifiers(): this.type

  /** Add modifiers to generation process, overriding any modifiers present on type itself */
  def modifiers(modifiers: ModifierSet): this.type

  /** Add generation path for nested serializer */
  def generationPath(path: GenerationPath*): this.type

  /** @return Reference for nested serializer */
  def build(): NestedResult[T]
}
