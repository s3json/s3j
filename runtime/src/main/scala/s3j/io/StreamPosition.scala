package s3j.io

case class StreamPosition(offset: Int, line: Int, column: Int, context: String, contextOffset: Int)
