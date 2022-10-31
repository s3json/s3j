package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object UnknownKeysModifier extends ModifierKey[UnknownKeysModifier]("unknownKeys") {
  override def default: Option[UnknownKeysModifier] = Some(UnknownKeysModifier(false))
}

case class UnknownKeysModifier(allow: Boolean) extends Modifier {
  def key: ModifierKey[UnknownKeysModifier] = UnknownKeysModifier
}
