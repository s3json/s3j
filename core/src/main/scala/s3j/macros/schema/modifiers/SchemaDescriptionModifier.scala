package s3j.macros.schema.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object SchemaDescriptionModifier extends ModifierKey[SchemaDescriptionModifier]("schemaDescription")

case class SchemaDescriptionModifier(description: String) extends Modifier {
  def key: ModifierKey[SchemaDescriptionModifier] = SchemaDescriptionModifier
  override def inheritable: Boolean = false
}
