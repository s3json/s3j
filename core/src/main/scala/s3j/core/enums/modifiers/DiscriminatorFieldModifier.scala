package s3j.core.enums.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object DiscriminatorFieldModifier extends ModifierKey[DiscriminatorFieldModifier]("discriminatorField")

case class DiscriminatorFieldModifier(name: String) extends Modifier {
  def key: ModifierKey[DiscriminatorFieldModifier] = DiscriminatorFieldModifier
}
