package s3j.core.binary

import s3j.macros.modifiers.{Modifier, ModifierKey}

object BinaryFormatModifier extends ModifierKey[BinaryFormatModifier]("binaryFormat")

case class BinaryFormatModifier(format: BinaryFormat) extends Modifier {
  def key: ModifierKey[BinaryFormatModifier] = BinaryFormatModifier
}
