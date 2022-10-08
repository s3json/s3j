package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object FieldKeyModifier {
  val key: ModifierKey[FieldKeyModifier] = ModifierKey("fieldKey")
}

case class FieldKeyModifier(fieldKey: String) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = FieldKeyModifier.key

  /** Field keys are meant for a very specific field and therefore are not inheritable */
  override def inheritable: Boolean = false
}
