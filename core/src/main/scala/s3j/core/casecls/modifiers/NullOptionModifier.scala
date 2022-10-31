package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

case object NullOptionModifier extends ModifierKey[NullOptionModifier.type]("nullOption") with Modifier {
  val key: ModifierKey[NullOptionModifier.type] = this
}
