package s3j.io

import s3j.JsPath

object JsonLocation {
  case object UnknownLocation extends JsonLocation {
    def toPath: Option[JsPath] = None
  }

  case class TreeLocation(path: JsPath) extends JsonLocation {
    def toPath: Option[JsPath] = Some(path)
  }

  case class StreamLocation(pos: StreamPosition, path: JsPath) extends JsonLocation {
    def toPath: Option[JsPath] = Some(path)
  }
}

/** Object pointing to a specific place in JSON document where something has occurred */
sealed trait JsonLocation {
  def toPath: Option[JsPath]
}
