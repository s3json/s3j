package s3j.schema.model

import s3j.annotations.discriminator
import s3j.internal.InternalMacros
import s3j.{*, given}

enum StringEncodingSchema derives JsonFormat {
  /** ASCII string (which does not need any separate transport encoding) */
  @discriminator("7bit")
  case SevenBit

  /** 8-bit string (whatever that may mean in context of unicode strings) */
  @discriminator("8bit")
  case EightBit

  /** Alias for 8-bit encoding */
  case Binary

  /** Quoted-printable encoding (RFC 2048, section 6.7) */
  case QuotedPrintable

  /** Hexadecimal (base16) encoding (RFC 4648, section 8) */
  case Base16

  /** Base32 encoding (RFC 4648, section 6) */
  case Base32

  /** Base64 encoding (RFC 4648, section 4) */
  case Base64

  /** URL-safe base64 encoding (non-standard value, RFC 4846, section 5) */
  case Base64Url

  /** Custom value not modelled by this enumeration */
  case Custom(value: String)
}
