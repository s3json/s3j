package s3j.core.enums

import s3j.annotations.naming.CaseConvention
import s3j.core.enums.modifiers.{DiscriminatorModifier, EnumCaseModifier}
import s3j.macros.modifiers.ModifierSet

import scala.quoted.Quotes

object EnumUtils {
  def discriminator(using q: Quotes)(sym: q.reflect.Symbol, modifiers: ModifierSet): String =
    modifiers.get(DiscriminatorModifier.key).map(_.name).getOrElse {
      val cc = modifiers.get(EnumCaseModifier.key).fold(CaseConvention.NoConvention)(_.value)
      CaseConvention.transform(cc, sym.name)
    }
}
