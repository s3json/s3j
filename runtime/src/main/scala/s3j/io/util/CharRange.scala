package s3j.io.util

import java.nio.CharBuffer
import scala.collection.mutable

object CharRange {
  val empty: CharRange = new CharRange(0)
}

/**
 * A tiny wrapper around single char array and position/limit fields.
 *
 * While [[CharBuffer]] has a convenient, extensive and beautiful API (especially on latest JVMs), due to it's over-
 * abstraction it is too easy to accidentally hit a generic method without specialization that will degrade your
 * otherwise super-fast implementation to something that is slower than virtually any existing library. Just like
 * `CharBuffer.put(CharBuffer)` method did, degrading to virtual-put-and-get loop.
 *
 * This class is intentionally restricted to operate directly on arrays with zero offset.
 *
 * API of this class tries to mimic API of char buffers. Methods with same names will have mostly same semantics.
 */
final class CharRange(val data: Array[Char]) {
  def this(capacity: Int) = this(new Array[Char](capacity))

  private val _data: Array[Char] = data
  private var _position: Int = 0
  private var _limit: Int = 0

  // Accessors:

  def position: Int = _position
  def position_=(x: Int): Unit = _position = x
  def limit: Int = _limit
  def limit_=(x: Int): Unit = _limit = x
  def capacity: Int = _data.length
  def remaining: Int = _limit - _position
  def hasRemaining: Boolean = _position < _limit

  /** Reset position to zero and limit to capacity, freeing up buffer for write operations */
  def clear(): CharRange = {
    _position = 0
    _limit = capacity
    this
  }

  /** Set position to zero and limit to previous position, turning write buffer into read buffer */
  def flip(): CharRange = {
    _limit = _position
    _position = 0
    this
  }

  /** Copy remaining contents of buffer into beginning, adjust position and limit */
  def compact(): CharRange = {
    val length = _limit - _position
    System.arraycopy(_data, _position, _data, 0, length)
    _position = 0
    _limit = length
    this
  }

  /** Compact the buffer, keeping `n` characters of prefix in front */
  def compactPrefix(n: Int): CharRange = {
    assert(n <= _position)
    _position -= n
    compact()
    _position += n
    this
  }

  /** Create copy of this range, sharing the data array but maintaining different positions and limits */
  def duplicate(): CharRange = {
    val r = new CharRange(_data)
    r._position = _position
    r._limit = _limit
    r
  }

  /** Adjusts buffer position by specified delta */
  def adjustPosition(n: Int): CharRange = { _position += n; this }

  /** @return Character at current position; position is post-incremented by one */
  def get(): Char = {
    val r = _data(_position)
    _position += 1
    r
  }

  /** @return Character at current position; position is left unchanged */
  def head: Char = _data(_position)

  /** Copy data into target buffer, adjust position */
  def get(target: Array[Char], offset: Int, length: Int): CharRange = {
    System.arraycopy(_data, _position, target, offset, length)
    _position += length
    this
  }
  
  /** Put the given char sequence at current position and advance position by length of the sequence */
  def put(s: CharSequence): CharRange = put(s, 0, s.length())

  /** Put the subsequence of a given char sequence at current position and advance position by length */
  def put(s: CharSequence, offset: Int, length: Int): CharRange = {
    s match {
      case s: String => 
        s.getChars(offset, offset + length, _data, _position)
      
      case b: CharBuffer => 
        val pos = b.position()
        try b.position(pos + offset).get(_data, _position, length)
        finally b.position(pos)

      case sb: java.lang.StringBuilder =>
        sb.getChars(offset, offset + length, _data, _position)

      case _ =>
        var i = 0
        while (i < length) {
          _data(_position + i) = s.charAt(offset + i)
          i += 1
        }
    }

    _position += length
    this
  }
  
  /** Put the char array at current position and advance position by length */
  def put(c: Array[Char], offset: Int, length: Int): CharRange = {
    System.arraycopy(c, offset, _data, _position, length)
    _position += length
    this
  }
  
  /** Append readable contents of this buffer to [[StringBuilder]], without consuming it */
  def appendTo(sb: mutable.StringBuilder): Unit = sb.appendAll(data, _position, _limit - _position)
  
  /** @return Contents of a readable part of the buffer */
  override def toString: String = new String(_data, _position, _limit - _position)
}
