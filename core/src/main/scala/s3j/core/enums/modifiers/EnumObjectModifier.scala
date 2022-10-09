package s3j.core.enums.modifiers

import s3j.macros.modifiers.{Modifier, ModifierKey}

object EnumObjectModifier {
  enum Behavior {
    case Default        // enums with all singletons are serialized as strings, otherwise all cases are objects
    case AllowStrings   // singleton enum cases are serialized as strings, non-singletons are objects
    case ForceObject    // all cases are objects
  }

  val key: ModifierKey[EnumObjectModifier] = ModifierKey("enumObjectBehavior")
}

case class EnumObjectModifier(behavior: EnumObjectModifier.Behavior) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = EnumObjectModifier.key
}
