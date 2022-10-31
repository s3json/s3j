package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object FieldKeyModifier extends ModifierKey[FieldKeyModifier]("fieldKey")

case class FieldKeyModifier(fieldKey: String) extends Modifier {
  def key: ModifierKey[FieldKeyModifier] = FieldKeyModifier

  /** Field keys are meant for a very specific field and therefore are not inheritable */
  override def inheritable: Boolean = false
}
