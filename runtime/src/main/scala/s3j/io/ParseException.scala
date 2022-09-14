package s3j.io

import s3j.JsPath

case class ParseException(location: JsonLocation, reason: String)
extends RuntimeException(s"Parse failure [$location]: $reason")
