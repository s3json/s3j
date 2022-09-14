package s3j.io

type JsonToken = Int

object JsonToken {
  // Do not place type annotations in 'final val's, otherwise constant inlining will break and severely hit performance
  // for same reason JsonToken cannot be an opaque type

  final val TEndOfStream      = 0     // end of stream has been reached
  final val TObjectStart      = 1     // start of object  '{'
  final val TArrayStart       = 2     // start of array   '['
  final val TStructureEnd     = 3     // end of structure (array or object)
  final val TTrueValue        = 4     // 'true' literal
  final val TFalseValue       = 5     // 'false' literal
  final val TNullValue        = 6     // 'null' literal
  final val TKey              = 7     // keys are special beasts, they are never chunked
  final val TStringContinued  = 8     // string fragment, continued on the next chunk
  final val TString           = 9     // string fragment, last chunk
  final val TNumberContinued  = 10    // number fragment, continued on the next chunk
  final val TNumber           = 11    // number fragment, last chunk

  /** @return Human-readable name of a token */
  def tokenName(t: JsonToken): String = _tokenNames(t)

  private val _tokenNames: Array[String] = Array(
    "EndOfStream", "ObjectStart", "ArrayStart", "StructureEnd", "TrueValue", "FalseValue", "NullValue", "Key",
    "StringContinued", "String", "NumberContinued", "Number"
  )
}
