package s3j.format.util

import s3j.io.JsonReader

case class DiscriminatorResult(reader: JsonReader, discriminator: String)
