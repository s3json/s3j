package s3j.io.util

class InvalidEscapeException(val position: Int, val length: Int, val message: String)
extends RuntimeException {
  override def getMessage: String = s"Invalid escape sequence: $message at position $position"
  override def fillInStackTrace(): Throwable = this
}
