package s3j.core.enums.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object AllowBufferingModifier extends Modifier {
  val key: ModifierKey[AllowBufferingModifier.type] = ModifierKey("allowBuffering")
}
