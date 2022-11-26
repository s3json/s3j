package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object KeyPrefixModifier extends ModifierKey[KeyPrefixModifier]("keyPrefix")

case class KeyPrefixModifier(prefix: String) extends Modifier {
  def key: ModifierKey[_ >: KeyPrefixModifier.this.type <: Modifier] = KeyPrefixModifier
}
