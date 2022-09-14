package s3j.io.util

import JsonLexer._

private[util] object LexerConst {
  // Lexer states:
  final val SNormal = 0
  final val SString = 1
  final val SNumber = 2

  // Table switch on sequential character class is much more efficient than lookup switch on sparse character values
  // Char class has three lower bits allocated to class type and five upper bits for attached value
  val CharClasses: Array[Byte] = new Array[Byte](256)

  final val ClTypeMask = 0x07
  final val ClValueShift = 3
  final val ClValueMask = 0x1F

  // Class types:
  //  - CtInvalid     - character is invalid in JSON stream (default state for character)
  //  - CtDirect      - character is a token on it's own, value encodes the corresponding token
  //  - CtStateSwitch - character switches tokenizer state (string or number start)
  //  - CtWhitespace  - whitespace character
  //  - CtLiteral     - token signals start of literal
  final val CtInvalid = 0
  final val CtDirect = 1
  final val CtStateSwitch = 2
  final val CtWhitespace = 3
  final val CtLiteral = 4

  // State switch constants:
  final val CvsNumberStart = 0
  final val CvsNumberPart = 1
  final val CvsString = 2

  // Literal constants:
  final val CvlTrue = 0
  final val CvlFalse = 1
  final val CvlNull = 2

  val TokenNames: Array[String] = Array(
    "EndOfStream", "BeginObject", "BeginArray", "EndObject", "EndArray", "Comma", "Colon",
    "TrueValue", "FalseValue", "NullValue", "StringContinued", "String", "NumberContinued", "Number"
  )

  val LiteralText: Array[String] = Array("true", "false", "null")

  // Continuation that should follow specific literal (first letter is already parsed)
  val LiteralData: Array[Array[Char]] = LiteralText.map(_.tail.toCharArray)

  // Output tokens for specific literal
  val LiteralTokens: Array[Int] = Array(LTTrueValue, LTFalseValue, LTNullValue)

  // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- // -- //
  // Init code:

  private def makeClass(t: Int, v: Int): Byte = (t | (v << ClValueShift)).toByte

  inline def charClass(inline c: Char): Byte = if (c < 256) CharClasses(c) else CtInvalid

  inline def cvsIsNumber(inline x: Int): Boolean = (x & 0xE) == CvsNumberStart

  inline def classType(inline x: Byte): Int = x & ClTypeMask

  inline def classValue(inline x: Byte): Int = (x >> ClValueShift) & ClValueMask

  CharClasses('{') = makeClass(CtDirect, LTBeginObject)
  CharClasses('}') = makeClass(CtDirect, LTEndObject)
  CharClasses('[') = makeClass(CtDirect, LTBeginArray)
  CharClasses(']') = makeClass(CtDirect, LTEndArray)
  CharClasses(',') = makeClass(CtDirect, LTComma)
  CharClasses(':') = makeClass(CtDirect, LTColon)

  CharClasses('t') = makeClass(CtLiteral, CvlTrue)
  CharClasses('f') = makeClass(CtLiteral, CvlFalse)
  CharClasses('n') = makeClass(CtLiteral, CvlNull)

  CharClasses('"') = makeClass(CtStateSwitch, CvsString)

  for (c <- " \t\n\r") CharClasses(c) = CtWhitespace
  for (c <- "0123456789-") CharClasses(c) = makeClass(CtStateSwitch, CvsNumberStart)
  for (c <- "eE+.") CharClasses(c) = makeClass(CtStateSwitch, CvsNumberPart)
}
