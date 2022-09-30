package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object UnknownKeysModifier {
  val key: ModifierKey[UnknownKeysModifier] = ModifierKey("unknownKeys", UnknownKeysModifier(false))
}

case class UnknownKeysModifier(allow: Boolean) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = UnknownKeysModifier.key
}
