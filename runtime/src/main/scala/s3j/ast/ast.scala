package s3j.ast

import s3j.format.{BasicFormats, JsonDecoder}
import s3j.format.BasicFormats.jsValueFormat
import s3j.io.AstJsonReader
import s3j.io.IoExtensions.toJsonString

import scala.annotation.targetName
import scala.language.implicitConversions

// N.B.: Single-file limitation of sealed traits is really annoying

object JsValue {
  export TreeBoxing.given
}

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
  export TreeBoxing.{jsBooleanBox, jsBooleanUnbox}

  // 'new' is required to avoid invocation of overloaded apply() fn
  val JsFalse: JsBoolean = new JsBoolean(false)
  val JsTrue: JsBoolean = new JsBoolean(true)

  /** @return Instance of [[JsBoolean]] representing passed value */
  def apply(b: Boolean): JsBoolean = if (b) JsTrue else JsFalse
}

/** Boolean JSON AST node. */
final case class JsBoolean(value: Boolean) extends JsValue

object JsNumber {
  export TreeBoxing.{jsNumberBoxInt, jsNumberBoxLong, jsNumberBoxFloat, jsNumberBoxDouble, jsNumberBoxBigInt,
    jsNumberBoxBigDecimal, jsNumberUnboxInt, jsNumberUnboxLong, jsNumberUnboxFloat, jsNumberUnboxDouble,
    jsNumberUnboxBigInt, jsNumberUnboxBigDecimal}

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

object JsInt {
  export TreeBoxing.{jsNumberBoxInt, jsNumberUnboxInt}
}

final case class JsInt(value: Int) extends JsNumber {
  def toInt: Int = value
  def toLong: Long = value
  def toFloat: Float = value
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value)
  def toBigDecimal: BigDecimal = BigDecimal(value, 0)
  override def toString: String = value.toString

  override def hashCode(): Int = value
  override def equals(x: Any): Boolean = JsLong.longEquals(value, x)
}

object JsLong {
  export TreeBoxing.{jsNumberBoxLong, jsNumberUnboxLong}

  private[ast] def longEquals(value: Long, x: Any): Boolean = x match {
    case JsInt(x)     => value == x
    case JsLong(x)    => value == x
    case JsFloat(x)   => value == x
    case JsDouble(x)  => value == x

    case n: JsNumber =>
      val v = n.toBigDecimal
      v.isValidLong && v.toLong == value

    case _ => false
  }
}

final case class JsLong(value: Long) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value
  def toFloat: Float = value.toFloat
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value)
  def toBigDecimal: BigDecimal = BigDecimal(value, 0)
  override def toString: String = value.toString

  override def hashCode(): Int = (value ^ (value >>> 32)).toInt
  override def equals(x: Any): Boolean = JsLong.longEquals(value, x)
}

object JsFloat {
  export TreeBoxing.{jsNumberBoxFloat, jsNumberUnboxFloat}
}

final case class JsFloat(value: Float) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value.toLong)
  def toBigDecimal: BigDecimal = BigDecimal(value)
  override def toString: String = value.toString

  override def hashCode(): Int = java.lang.Double.hashCode(value)
  override def equals(x: Any): Boolean = JsDouble.doubleEquals(value, x)
}

object JsDouble {
  export TreeBoxing.{jsNumberBoxDouble, jsNumberUnboxDouble}

  private[ast] def doubleEquals(value: Double, x: Any): Boolean = x match {
    case JsInt(x)     => value == x
    case JsLong(x)    => value == x
    case JsFloat(x)   => value == x
    case JsDouble(x)  => value == x
    case n: JsNumber  => n.toDouble == value

    case _ => false
  }
}

final case class JsDouble(value: Double) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
  def toDouble: Double = value
  def toBigInt: BigInt = BigInt(value.toLong)
  def toBigDecimal: BigDecimal = BigDecimal(value)
  override def toString: String = value.toString

  override def hashCode(): Int = java.lang.Double.hashCode(value)
  override def equals(x: Any): Boolean = JsDouble.doubleEquals(value, x)
}

object JsBigInt {
  export TreeBoxing.{jsNumberBoxBigInt, jsNumberUnboxBigInt}
}

final case class JsBigInt(value: BigInt) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
  def toDouble: Double = value.toDouble
  def toBigInt: BigInt = value
  def toBigDecimal: BigDecimal = BigDecimal(value, 0)
  override def toString: String = value.toString

  override def hashCode(): Int = value.hashCode()
  override def equals(x: Any): Boolean = x match {
    case JsInt(x)     => value.isValidInt && value.toInt == x
    case JsLong(x)    => value.isValidLong && value.toLong == x
    case JsFloat(x)   => value.toFloat == x
    case JsDouble(x)  => value.toDouble == x
    case JsBigInt(x)  => value == x
    case JsBigDecimal(x) => x.toBigIntExact.contains(value)
    case x: JsNumber => value == x.toBigInt
    case _ => false
  }
}

object JsBigDecimal {
  export TreeBoxing.{jsNumberBoxBigDecimal, jsNumberUnboxBigDecimal}
}

final case class JsBigDecimal(value: BigDecimal) extends JsNumber {
  def toInt: Int = value.toInt
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
  def toDouble: Double = value.toDouble
  def toBigInt: BigInt = value.toBigInt
  def toBigDecimal: BigDecimal = value
  override def toString: String = value.toString

  override def hashCode(): Int = value.hashCode()
  override def equals(x: Any): Boolean = x match {
    case JsInt(x)     => value.isValidInt && value.toInt == x
    case JsLong(x)    => value.isValidLong && value.toLong == x
    case JsFloat(x)   => value.toFloat == x
    case JsDouble(x)  => value.toDouble == x
    case JsBigInt(x)  => value.toBigIntExact.contains(x)
    case x: JsNumber  => value == x.toBigDecimal
    case _ => false
  }
}

object JsString {
  export TreeBoxing.{jsStringBox, jsStringUnbox}
}

/** String JSON AST node */
final case class JsString(value: String) extends JsValue

/** Array JSON AST node */
object JsArray {
  def apply(xs: JsValue*): JsArray = new JsArray(xs)
}

final class JsArray(val value: Seq[JsValue]) extends JsValue {
  def apply(i: Int): JsValue = value(i)
  def foreach[U](f: JsValue => U): Unit = value.foreach(f)
  def iterator: Iterator[JsValue] = value.iterator
  def zipWithIndex: Seq[(JsValue, Int)] = value.zipWithIndex

  override def hashCode(): Int = value.hashCode()
  override def equals(x: Any): Boolean = x match {
    case arr: JsArray => value == arr.value
    case _ => false
  }
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
final class JsObject(val items: Map[String, JsValue], val order: Seq[String] = Nil) extends JsValue {
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

  // Field ordering is excluded from equality check, as it's for aesthetics only
  override def hashCode(): Int = items.hashCode()
  override def equals(x: Any): Boolean = x match {
    case obj: JsObject => items == obj.items
    case _ => false
  }
}
