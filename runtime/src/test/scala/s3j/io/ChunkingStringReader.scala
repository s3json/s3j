package s3j.io

import s3j.io.util.CharRange

/** Special JSON reader that outputs single string in chunks, for testing streaming decoders */
class ChunkingStringReader(chunks: Seq[String]) extends JsonReader {
  val chunk: CharRange = new CharRange(chunks.map(_.length).max)
  private var chunkIdx = 0

  protected def readToken(): JsonToken = {
    chunk.clear().put(chunks(chunkIdx)).flip()
    chunkIdx += 1
    if (chunkIdx < chunks.length) JsonToken.TStringContinued else JsonToken.TString
  }

  def key: KeyHandle = throw new UnsupportedOperationException("ChunkedStringReader.keyHandle")
  def location: JsonLocation = JsonLocation.UnknownLocation
}
