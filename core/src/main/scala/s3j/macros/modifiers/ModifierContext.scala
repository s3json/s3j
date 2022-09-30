package s3j.macros.modifiers

object ModifierContext {
  /** Annotation was found on generic structures (such as objects) */
  case object Generic extends ModifierContext

  /** Annotation was found on enumeration outer object */
  case object Enum extends ModifierContext

  /** Annotation was found on enumeration case */
  case object EnumCase extends ModifierContext

  /** Annotation was found on case class field */
  case object Field extends ModifierContext
}

/**
 * Context where modifier has appeared:
 */
sealed trait ModifierContext
