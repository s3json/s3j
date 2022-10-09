package s3j.core.enums.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object DiscriminatorFieldModifier {
  val key: ModifierKey[DiscriminatorFieldModifier] = ModifierKey("discriminatorField")
}

case class DiscriminatorFieldModifier(name: String) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = DiscriminatorFieldModifier.key
}
