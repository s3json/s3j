package s3j.io.util

import s3j.JsPath
import s3j.io.KeyHandle

object JsPathBuilder {
  private final val NArray        = 0x8000_0000
  private final val NLevelValid  = 0x4000_0000
  private final val NMask         = 0x3FFF_FFFF

  final val RRoot   = 0
  final val RObject = 1
  final val RArray  = 2
}

/**
 * Mutable structure to efficiently track current JSON path. Also serves as a key handle for most recent object.
 */
class JsPathBuilder(maxKeyLength: Int, maxNesting: Int) {
  import JsPathBuilder.*

  /**
   * Encoding:
   *  keyData is a continuous array, e.g. keys are placed tightly after each other
   *  levelData describes each level:
   *    if TArray bit flag is set, then remaining bits represent array index
   *    otherwise, remaining bits represent end index of key data (points to next empty char)
   *
   * levelCount - number of active nesting levels
   */
  private var keyData: Array[Char] = new Array[Char](math.min(256, maxKeyLength))
  private val levelData: Array[Int] = new Array[Int](maxNesting)
  private var levelCount: Int = 0

  private var prevKeyOffset: Int = 0
  private var thisKeyOffset: Int = 0  // when levelCount > 0 equals to levelData(levelCount - 1)
  private val _keyHandle: KeyHandle.CharArrayHandle = new KeyHandle.CharArrayHandle

  def key: KeyHandle = _keyHandle

  private def checkPush(): Unit = {
    if (levelCount == maxNesting) {
      throw new IllegalArgumentException("Maximum nesting level exceeded: " + maxNesting)
    }
  }

  /** Start new array with zero index */
  def pushArray(): Unit = {
    checkPush()
    levelData(levelCount) = NArray
    levelCount += 1
  }

  /** Increment index on current array */
  def incrementIndex(): Unit = {
    val idx = levelData(levelCount - 1) & NMask
    levelData(levelCount - 1) = NArray | NLevelValid | (idx + 1)
  }

  /** Start new object with empty key */
  def pushObject(): Unit = {
    checkPush()

    prevKeyOffset = thisKeyOffset
    levelData(levelCount) = 0
    levelCount += 1
  }

  /** Clear buffered key of current object, making it ready to push new data */
  def resetKey(): Unit = {
    thisKeyOffset = prevKeyOffset
    levelData(levelCount - 1) = thisKeyOffset
    _keyHandle.set(keyData, prevKeyOffset, 0)
  }

  /** Append key chunk to current object */
  def appendKey(chunk: CharRange): Unit = {
    if (thisKeyOffset + chunk.remaining > maxKeyLength) {
      throw new IllegalArgumentException("Total key length exceeded (" + maxKeyLength + ")")
    }

    if (thisKeyOffset + chunk.remaining > keyData.length) {
      keyData = java.util.Arrays.copyOf(keyData, math.min(maxKeyLength, keyData.length * 2))
    }

    System.arraycopy(chunk.data, chunk.position, keyData, thisKeyOffset, chunk.remaining)
    thisKeyOffset += chunk.remaining
    _keyHandle.set(keyData, prevKeyOffset, thisKeyOffset - prevKeyOffset)
    levelData(levelCount - 1) = thisKeyOffset
  }

  /** Mark this level as valid */
  def finishLevel(): Unit =
    levelData(levelCount - 1) |= NLevelValid

  /**
   * End current nesting level
   * @return Type of now active level (see `R*` constants)
   */
  def pop(): Int = {
    levelCount -= 1

    if ((levelData(levelCount) & NArray) == 0) {
      thisKeyOffset = prevKeyOffset

      var i = levelCount - 2
      while (i >= 0 && (levelData(i) & NArray) != 0) i -= 1

      prevKeyOffset = if (i >= 0) levelData(i) & NMask else 0
      _keyHandle.set(keyData, prevKeyOffset, thisKeyOffset - prevKeyOffset)
    }

    if (levelCount == 0) {
      return JsPathBuilder.RRoot
    }

    if ((levelData(levelCount - 1) & NArray) == 0) JsPathBuilder.RObject
    else JsPathBuilder.RArray
  }

  /** Materialize JsPath instance from current state */
  def toPath: JsPath = {
    var r: JsPath = JsPath.Root
    var lastKeyOffset: Int = 0

    for (i <- 0 until levelCount if (levelData(i) & NLevelValid) != 0) {
      val n = levelData(i)
      if ((n & NArray) != 0) {
        r = JsPath.Array(r, (n & NMask) - 1)
      } else {
        val ofs = n & NMask
        r = JsPath.Object(r, new String(keyData, lastKeyOffset, ofs - lastKeyOffset))
        lastKeyOffset = ofs
      }
    }

    r
  }
}
