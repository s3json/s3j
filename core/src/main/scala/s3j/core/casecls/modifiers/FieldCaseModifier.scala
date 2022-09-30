package s3j.core.casecls.modifiers

import s3j.annotations.naming.CaseConvention
import s3j.macros.modifiers.{Modifier, ModifierKey}

object FieldCaseModifier {
  val key: ModifierKey[FieldKeyModifier] = ModifierKey("fieldCase")
}

case class FieldCaseModifier(value: CaseConvention) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = FieldCaseModifier.key
}