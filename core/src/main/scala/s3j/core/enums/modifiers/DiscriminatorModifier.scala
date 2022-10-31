package s3j.core.enums.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object DiscriminatorModifier extends ModifierKey[DiscriminatorModifier]("discriminator")

case class DiscriminatorModifier(name: String) extends Modifier {
  def key: ModifierKey[DiscriminatorModifier] = DiscriminatorModifier
}
