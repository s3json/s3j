package s3j.macros.modifiers

object BuiltinModifiers {
  val requireImplicit: ModifierKey[RequireImplicit.type] = ModifierKey("requireImplicit")

  /**
   * This modifier is actually handled by macro system itself, as it prevents any plugins from being queried during
   * generation of annotated type.
   */
  case object RequireImplicit extends Modifier {
    def key: ModifierKey[_ <: RequireImplicit.this.type] = requireImplicit
  }
}
