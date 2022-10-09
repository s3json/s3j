package s3j.core.numbers

import s3j.macros.modifiers.{Modifier, ModifierKey}

case object UnsignedModifier extends Modifier {
  val key: ModifierKey[UnsignedModifier.type] = ModifierKey("unsigned")
  override def suppressImplicitSearch: Boolean = true
}
