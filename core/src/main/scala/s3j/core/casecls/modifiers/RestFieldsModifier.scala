package s3j.core.casecls.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

case object RestFieldsModifier extends Modifier {
  val key: ModifierKey[RestFieldsModifier.type] = ModifierKey("restFields")
}
