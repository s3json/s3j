package s3j.core.primitives

import s3j.macros.modifiers.{Modifier, ModifierKey}

case object UnsignedModifier extends ModifierKey[UnsignedModifier.type]("unsigned") with Modifier {
  val key: ModifierKey[UnsignedModifier.type] = this
}
