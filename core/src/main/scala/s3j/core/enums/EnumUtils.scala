package s3j.core.enums

import s3j.core.enums.modifiers.{DiscriminatorModifier, EnumCaseModifier}
import s3j.macros.generic.CaseConvention
import s3j.macros.modifiers.ModifierSet

import scala.quoted.Quotes

object EnumUtils {
  def discriminator(using q: Quotes)(sym: q.reflect.Symbol, modifiers: ModifierSet): String =
    modifiers.get(DiscriminatorModifier.key).map(_.name).getOrElse {
      modifiers
        .get(EnumCaseModifier.key)
        .fold(CaseConvention.NoConvention)(_.value)
        .transform(sym.name)
    }
}
