package s3j.macros.utils

object GenerationPath {
  case class ObjectField(field: String) extends GenerationPath

}

sealed trait GenerationPath
