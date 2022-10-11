package s3j.io

object Scratchpad {
  private val _tl: ThreadLocal[Scratchpad] =
    new ThreadLocal[Scratchpad] { override def initialValue(): Scratchpad = new Scratchpad }

  def get(): Scratchpad = _tl.get()
}

class Scratchpad {
  val chars: Array[Char] = new Array[Char](1024)
  val bytes: Array[Byte] = new Array[Byte](2048)
}
