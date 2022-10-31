package s3j.macros.modifiers

/**
 * Abstraction over annotations used to alter serializer behavior. Multiple mutually exclusive annotation may map into
 * same modifier type.
 */
trait Modifier {
  /** @return Key assigned to that modifier */
  def key: ModifierKey[_ >: this.type <: Modifier]
  
  /** @return Whether this modifier could be inherited from parent symbols */
  def inheritable: Boolean = true
}
