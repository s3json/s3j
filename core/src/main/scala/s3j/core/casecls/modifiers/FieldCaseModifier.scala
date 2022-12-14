package s3j.core.casecls.modifiers

import s3j.macros.generic.CaseConvention
import s3j.macros.modifiers.{Modifier, ModifierKey}

object FieldCaseModifier extends ModifierKey[FieldCaseModifier]("fieldCase") {
  override val default: Option[FieldCaseModifier] = Some(FieldCaseModifier(CaseConvention.NoConvention))
}

case class FieldCaseModifier(value: CaseConvention) extends Modifier {
  def key: ModifierKey[FieldCaseModifier] = FieldCaseModifier
}
