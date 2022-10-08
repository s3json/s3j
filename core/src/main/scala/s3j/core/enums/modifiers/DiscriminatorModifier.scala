package s3j.core.enums.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object DiscriminatorModifier {
  val key: ModifierKey[DiscriminatorModifier] = ModifierKey("discriminator")
}

case class DiscriminatorModifier(name: String) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = DiscriminatorModifier.key
}
