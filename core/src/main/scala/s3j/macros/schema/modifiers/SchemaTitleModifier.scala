package s3j.macros.schema.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object SchemaTitleModifier extends ModifierKey[SchemaTitleModifier]("schemaTitle")

case class SchemaTitleModifier(title: String) extends Modifier {
  def key: ModifierKey[SchemaTitleModifier] = SchemaTitleModifier
  override def inheritable: Boolean = false
}
