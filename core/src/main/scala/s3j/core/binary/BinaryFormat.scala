package s3j.core.binary

import s3j.format.impl.BinaryEncoding

import scala.quoted.{Expr, Quotes}

object BinaryFormat {
  val Base64        = new BinaryFormat("base64",        '{ BinaryEncoding.Base64 })
  val Base64Url     = new BinaryFormat("base64url",     '{ BinaryEncoding.Base64Url })
  val HexLowercase  = new BinaryFormat("hexLowercase",  '{ BinaryEncoding.HexLowercase })
  val HexUppercase  = new BinaryFormat("hexUppercase",  '{ BinaryEncoding.HexUppercase })
}

class BinaryFormat(val name: String, val fn: Quotes ?=> Expr[BinaryEncoding]) {
  override def toString: String = name
}
