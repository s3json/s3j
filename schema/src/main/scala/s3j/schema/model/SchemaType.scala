package s3j.schema.model

import s3j.{*, given}

enum SchemaType derives JsonFormat {
  /** String schema type, specific keywords for this type are in [[StringSchema]] */
  case String

  /** Number schema type (floating), specific keywords for this type are in [[NumberSchema]] */
  case Number

  /** Integer schema type, specific keywords for this type are in [[NumberSchema]] */
  case Integer

  /** Object schema type, specific keywords for this type are in [[ObjectSchema]] */
  case Object
  
  /** Array schema type, specific keywords for this type are in [[ArraySchema]] */
  case Array
  
  /** Boolean schema type (no specific keywords for this type) */
  case Boolean
  
  /** Null schema type (no specific keywords for this type) */
  case Null

  /** Custom format not modelled by this enumeration */
  case Custom(value: String)
}
