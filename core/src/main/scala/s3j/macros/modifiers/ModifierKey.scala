package s3j.macros.modifiers

import scala.annotation.targetName

/**
 * Unique identification for [[Modifier]] instance.
 *
 * @param name Readable string for debugging
 * @tparam T   Type of the modifier
 */
class ModifierKey[T <: Modifier](val name: String) {
  def default: Option[T] = None

  final override def equals(obj: Any): Boolean = super.equals(obj)
  final override def hashCode(): Int = super.hashCode()
  override def toString: String = name
}
