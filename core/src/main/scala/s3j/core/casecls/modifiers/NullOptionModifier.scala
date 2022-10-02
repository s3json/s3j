package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

case object NullOptionModifier extends Modifier {
  val key: ModifierKey[_ <: Modifier] = ModifierKey("nullOption")
}
