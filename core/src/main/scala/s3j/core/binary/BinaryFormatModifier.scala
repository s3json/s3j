package s3j.core.binary

import s3j.macros.modifiers.{Modifier, ModifierKey}

object BinaryFormatModifier {
  val key: ModifierKey[BinaryFormatModifier] = ModifierKey("binaryFormat")
}

case class BinaryFormatModifier(format: BinaryFormat) extends Modifier {
  def key: ModifierKey[_ <: Modifier] = BinaryFormatModifier.key
}
