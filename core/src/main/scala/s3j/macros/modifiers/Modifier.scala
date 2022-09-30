package s3j.macros.modifiers

/**
 * Abstraction over annotations used to alter serializer behavior. Multiple mutually exclusive annotation may map into
 * same modifier type.
 */
trait Modifier {
  /** @return Key assigned to that modifier */
  def key: ModifierKey[_ <: Modifier]
  
  /** @return Whether presence of this modifier suppresses implicit search for type */
  def suppressImplicitSearch: Boolean = false
}
