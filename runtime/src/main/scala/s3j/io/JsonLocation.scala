package s3j.io

import s3j.JsPath

object JsonLocation {
  case object UnknownLocation extends JsonLocation
  case class TreeLocation(path: JsPath) extends JsonLocation
  case class StreamLocation(pos: StreamPosition, path: JsPath) extends JsonLocation
}

/** Object pointing to a specific place in JSON document where something (e.g. error) has been ocurred */
sealed trait JsonLocation {
}
