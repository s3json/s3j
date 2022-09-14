package s3j

import s3j.format
import s3j.format.JsonEncoder

// Extract important types to top level:

type JsonFormat[T]  = format.JsonFormat[T]
val  JsonFormat     = format.JsonFormat

type JsonEncoder[T] = format.JsonEncoder[T]
val  JsonEncoder    = format.JsonEncoder

type JsonDecoder[T] = format.JsonDecoder[T]
val  JsonDecoder    = format.JsonDecoder

// Bring extensions to scope:

export macros.MacroExtensions.*
export io.IoExtensions.*
