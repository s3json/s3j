package s3j.core.enums.modifiers

import s3j.macros.generic.CaseConvention
import s3j.macros.modifiers.{Modifier, ModifierKey}

object EnumCaseModifier extends ModifierKey[EnumCaseModifier]("enumCase")

case class EnumCaseModifier(value: CaseConvention) extends Modifier {
  def key: ModifierKey[EnumCaseModifier] = EnumCaseModifier
}
