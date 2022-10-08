package s3j.core.enums.modifiers

import s3j.annotations.naming.CaseConvention
import s3j.macros.modifiers.{Modifier, ModifierKey}

object EnumCaseModifier {
  val key: ModifierKey[EnumCaseModifier] = ModifierKey("enumCase")
}

case class EnumCaseModifier(value: CaseConvention) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = EnumCaseModifier.key
}
