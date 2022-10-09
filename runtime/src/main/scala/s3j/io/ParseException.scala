package s3j.io

import s3j.JsPath

case class ParseException(location: JsonLocation, reason: String, cause: Option[Throwable] = None)
extends RuntimeException(s"Parse failure [$location]: $reason") {
  for (c <- cause) initCause(c)
}
