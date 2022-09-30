package s3j.ast

import s3j.format.{BasicFormats, JsonDecoder}
import s3j.format.BasicFormats.jsValueFormat
import s3j.io.AstJsonReader
import s3j.io.IoExtensions.toJsonString

import scala.annotation.targetName
import scala.language.implicitConversions

// N.B.: Single-file limitation of sealed traits is really annoying

/**
 * Base class for any JSON AST entries
 */
sealed trait JsValue {
  def isObject: Boolean = this.isInstanceOf[JsObject]
  def isArray: Boolean = this.isInstanceOf[JsArray]

  def asObject: JsObject = this.asInstanceOf[JsObject]
  def asArray: JsArray = this.asInstanceOf[JsArray]
  def dynamic: JsDynamic = new JsDynamic(this)

  /** Decode serialized representation stored in this AST node */
  def convertTo[T](using dec: JsonDecoder[T]): T =
    dec.decode(new AstJsonReader(this))

  override def toString: String = this.toJsonString
}

case object JsNull extends JsValue

object JsBoolean {
  // 'new' is required to avoid invocation of overloaded apply() fn
  val JsFalse: JsBoolean = new JsBoolean(false)
  val JsTrue: JsBoolean = new JsBoolean(true)

  /** @return Instance of [[JsBoolean]] representing passed value */
  def apply(b: Boolean): JsBoolean = if (b) JsTrue else JsFalse
}

/** Boolean JSON AST node. */
case class JsBoolean(value: Boolean) extends JsValue

object JsNumber {
  def apply(i: Int): JsInt = JsInt(i)
  def apply(l: Long): JsLong = JsLong(l)
  def apply(f: Float): JsFloat = JsFloat(f)
  def apply(d: Double): JsDouble = JsDouble(d)
  def apply(i: BigInt): JsBigInt = JsBigInt(i)
  def apply(d: BigDecimal): JsBigDecimal = JsBigDecimal(d)
}

/**
 * Base class for number JSON AST nodes. Because there are so many numbers (and to avoid double-boxing them), this is
 * merely a sealed trait. `toXXX` methods could be used for convenient access.
 */
sealed trait JsNumber extends JsValue {
  def toInt: Int
  def toLong: Long
  def toFloat: Float
  def toDouble: Double
  def toBigInt: BigInt
  def toBigDecimal: BigDecimal
}

case class JsInt(value: Int) extends JsNumber {
  def toInt: Int = value
  def toLong: Long = value
  def toFloat: Float = value
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value)
  def toBigDecimal: BigDecimal = BigDecimal(value, 0)
  override def toString: String = value.toString
}

case class JsLong(value: Long) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value
  def toFloat: Float = value.toFloat
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value)
  def toBigDecimal: BigDecimal = BigDecimal(value, 0)
  override def toString: String = value.toString
}

case class JsFloat(value: Float) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value.toLong)
  def toBigDecimal: BigDecimal = BigDecimal(value)
  override def toString: String = value.toString
}

case class JsDouble(value: Double) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value.toLong)
  def toBigDecimal: BigDecimal = BigDecimal(value)
  override def toString: String = value.toString
}

case class JsBigInt(value: BigInt) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
  def toDouble: Double = value.toDouble
  def toBigInt: BigInt = value
  def toBigDecimal: BigDecimal = BigDecimal(value, 0)
  override def toString: String = value.toString
}

case class JsBigDecimal(value: BigDecimal) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
  def toDouble: Double = value.toDouble
  def toBigInt: BigInt = value.toBigInt
  def toBigDecimal: BigDecimal = value
  override def toString: String = value.toString
}

/** String JSON AST node */
case class JsString(value: String) extends JsValue

/** Array JSON AST node */
object JsArray {
  def apply(xs: JsValue*): JsArray = new JsArray(xs)
}

class JsArray(val value: Seq[JsValue]) extends JsValue {
  def apply(i: Int): JsValue = value(i)
  def foreach[U](f: JsValue => U): Unit = value.foreach(f)
  def iterator: Iterator[JsValue] = value.iterator
  def zipWithIndex: Seq[(JsValue, Int)] = value.zipWithIndex
}

object JsObject {
  /** Create new JsObject with ordered set of keys */
  def apply(kvs: (String, JsValue)*): JsObject = {
    val items = Map(kvs:_*)
    if (kvs.size == items.size) /* keys were unique */ new JsObject(items, kvs.view.map(_._1).toVector)
    else /* oh no, cringe */ new JsObject(items, kvs.view.distinct.map(_._1).toVector)
  }
}

/**
 * Object JSON AST node with ordered keys. Ordering is optional and could be disabled, saving some memory.
 *
 * Key ordering should be either complete (all keys listed) or absent (empty list). For performance reasons, no checks
 * are performed for validity of ordering array, and inconsistent data will result in incorrect behavior (missing keys or
 * spurious exceptions).
 */
class JsObject(val items: Map[String, JsValue], val order: Seq[String] = Nil) extends JsValue {
  /** Iterate contents of this object. Iteration respects key ordering */
  def foreach[U](f: ((String, JsValue)) => U): Unit = {
    if (order.isEmpty) items.foreach(f)
    else for (k <- order) f(k, items(k))
  }

  /** @return Iterator for keys in this object. Iteration respects key ordering */
  def keysIterator: Iterator[String] = if (order.isEmpty) items.keysIterator else order.iterator

  /** @return Iterator for this object. Iteration respects key ordering */
  def iterator: Iterator[(String, JsValue)] = new ObjectIterator(this)

  def has(key: String): Boolean = items.contains(key)
  def get(key: String): Option[JsValue] = items.get(key)
  def apply(key: String): JsValue = items(key)

  /** Add new key-value pair to the end of this object */
  @targetName("add")
  def + (kv: (String, JsValue)): JsObject = {
    if (items.nonEmpty && order.isEmpty) new JsObject(items + kv, Nil)
    else if (items.isEmpty) new JsObject(Map(kv), Vector(kv._1))
    else if (!items.contains(kv._1)) new JsObject(items + kv, order :+ kv._1)
    else new JsObject(items + kv, order.filterNot(_ == kv._1) :+ kv._1)
  }

  /** Add all key-value pairs to the end of this object in specified order */
  @targetName("addAll")
  def ++ (kvs: (String, JsValue)*): JsObject = {
    if (kvs.forall(kv => !items.contains(kv._1))) new JsObject(items ++ kvs, order ++ kvs.map(_._1))
    else new JsObject(items ++ kvs, order.filterNot(kvs.map(_._1).toSet) ++ kvs.map(_._1))
  }
}
