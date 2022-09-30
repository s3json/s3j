package s3j.macros.modifiers

import scala.annotation.targetName

object ModifierKey {
  /** @return Modifier key with given name and no default value */
  def apply[T <: Modifier](name: String): ModifierKey[T] = new ModifierKey[T](name, None)

  /** @return Modifier key with given name and specified default value */
  def apply[T <: Modifier](name: String, default: T): ModifierKey[T] = new ModifierKey[T](name, Some(default))
}

/**
 * Unique identification for [[Modifier]] instance.
 *
 * @param name Readable string for debugging
 * @tparam T
 */
final class ModifierKey[T <: Modifier](val name: String, val default: Option[T]) {
  override def toString: String = name
}
