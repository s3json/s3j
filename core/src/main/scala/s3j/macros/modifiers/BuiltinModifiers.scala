package s3j.macros.modifiers

object BuiltinModifiers {
  /**
   * This modifier is actually handled by macro system itself, as it prevents any plugins from being queried during
   * generation of annotated type.
   */
  case object RequireImplicit extends ModifierKey[RequireImplicit.type]("requireImplicit") with Modifier {
    def key: ModifierKey[RequireImplicit.type] = this
    override def inheritable: Boolean = false
  }

  case object InspectCodeModifier extends ModifierKey[InspectCodeModifier.type]("inspectCode") with Modifier {
    def key: ModifierKey[InspectCodeModifier.this.type] = this
    override def inheritable: Boolean = false
  }
}
