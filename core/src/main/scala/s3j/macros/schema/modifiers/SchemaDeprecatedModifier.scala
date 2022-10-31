package s3j.macros.schema.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object SchemaDeprecatedModifier extends ModifierKey[SchemaDeprecatedModifier.type]("schemaDeprecated") with Modifier {
  def key: ModifierKey[SchemaDeprecatedModifier.type] = this
  override def inheritable: Boolean = false
}
