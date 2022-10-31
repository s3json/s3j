package s3j.macros.schema.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object SchemaHiddenModifier extends ModifierKey[SchemaHiddenModifier.type]("schemaHidden") with Modifier {
  def key: ModifierKey[SchemaHiddenModifier.type] = this
  override def inheritable: Boolean = false
}
