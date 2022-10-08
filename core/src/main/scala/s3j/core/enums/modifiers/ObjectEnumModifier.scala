package s3j.core.enums.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

case object ObjectEnumModifier extends Modifier {
  def key: ModifierKey[_ <: Modifier] = ModifierKey("objectEnum")
}
