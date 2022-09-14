package s3j.io

object KeyHandle {
  class StringHandle extends KeyHandle {
    var str: String = _

    def stringEquals(s: String): Boolean = str == s
    override def stringHashCode: Int = str.hashCode
    override def toString: String = str
  }

  class CharArrayHandle extends KeyHandle {
    private var data: Array[Char] = _
    private var offset: Int = 0
    private var length: Int = 0

    private var stringInstance: String = _
    private var dataHash: Int = 0
    private var hashComputed: Boolean = false

    private def computeHash(): Unit = {
      var hash = 0
      var pos = offset
      val end = offset + length

      while (pos < end) {
        hash = hash * 31 + data(pos)
        pos += 1
      }

      dataHash = hash
      hashComputed = true
    }

    def set(data: Array[Char], offset: Int, length: Int): Unit = {
      this.data = data
      this.offset = offset
      this.length = length

      this.stringInstance = null
      this.hashComputed = false
    }

    def stringEquals(s: String): Boolean = toString.equals(s)

    def stringHashCode: Int = {
      if (!hashComputed) computeHash()
      dataHash
    }

    override def toString: String = {
      if (stringInstance == null) stringInstance = new String(data, offset, length)
      stringInstance
    }
  }
}

/**
 * Handle to access object key without (or with minimum) allocations. Instances of this trait are allowed to be mutable,
 * so any values are valid only until next call of reader methods.
 */
trait KeyHandle {
  /** @return Whether `toString == s`, but possibly computed more efficiently */
  def stringEquals(s: String): Boolean

  /** @return `toString.hashCode`, but possibly computed more efficiently */
  def stringHashCode: Int

  /** @return String value of this key */
  override def toString: String
}
