package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{ModifierKey, Modifier}

object InlineObjectModifier extends ModifierKey[InlineObjectModifier.type]("inlineObject") with Modifier {
  override def inheritable: Boolean = false
  def key: ModifierKey[_ >: InlineObjectModifier.this.type <: Modifier] = this
}
