package s3j.schema.model

import s3j.{*, given}

enum SchemaFormat derives JsonFormat {
  // Stringy formats:

  /** ISO8601 date and time format, i.e. `2018-11-13T20:20:39+00:00` */
  case DateTime

  /** ISO8601 time format, i.e. `20:20:39+00:00` */
  case Time

  /** ISO8601 date format, i.e. `2018-11-13` */
  case Date

  /** ISO8601 duration format, i.e. `P3D` */
  case Duration

  /** E-Mail address format (RFC 5321, section 4.1.2) */
  case Email

  /** Internationalized e-mail address format (RFC 6531) */
  case IdnEmail

  /** Hostname format (RFC 1123, section 2.1) */
  case Hostname

  /** Internationalized hostname format (RFC5890, section 2.3.2.3) */
  case IdnHostname

  /** IPv4 format (RFC 2673, section 3.2) */
  case Ipv4

  /** IPv6 format (RFC 2373, section 2.2) */
  case Ipv6

  /** UUID format (RFC 4122), i.e. `3e4666bf-d5e5-4aa7-b8ce-cefe41c7568a` */
  case Uuid

  /** URI format (RFC3986) */
  case Uri

  /** URI reference format (RFC3986, section 4.1) */
  case UriReference

  /** Internationalized URI format (RFC3987) */
  case Iri

  /** Internationalized URI reference format (RFC3987) */
  case IriReference

  /** URI template format (RFC6570) */
  case UriTemplate

  /** JSON pointer format (RFC6901) */
  case JsonPointer

  /** Relative JSON pointer format (RFC6901) */
  case RelativeJsonPointer

  /** Regular expression format */
  case Regex

  // OpenAPI specification formats

  /** Password field, a hint to UIs to mask the input */
  case Password

  /** Unsigned 8-bit integer */
  case Uint8

  /** Signed 8-bit integer */
  case Int8

  /** Unsigned 16-bit integer */
  case Uint16

  /** Signed 16-bit integer */
  case Int16

  /** Unsigned 32-bit integer */
  case Uint32

  /** Signed 32-bit integer */
  case Int32

  /** Unsigned 64-bit integer */
  case Uint64

  /** Signed 64-bit integer */
  case Int64

  /** Single-precision format */
  case Float

  /** Double-precision float */
  case Double

  /** Custom value not modelled by this enumeration */
  case Custom(value: String)
}
