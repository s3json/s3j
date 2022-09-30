package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object FieldKeyModifier {
  val key: ModifierKey[FieldKeyModifier] = ModifierKey("fieldKey")
}

case class FieldKeyModifier(fieldKey: String) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = FieldKeyModifier.key
}
