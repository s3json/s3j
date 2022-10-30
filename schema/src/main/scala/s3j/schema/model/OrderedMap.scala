package s3j.schema.model

import s3j.format.{JsonFormat, StringyFormat}
import s3j.format.util.ObjectFormatUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

object OrderedMap {
  given mapFormat[K, V](using kf: StringyFormat[K], vf: JsonFormat[V]): JsonFormat[OrderedMap[K, V]] with {
    def encode(writer: JsonWriter, value: OrderedMap[K, V]): Unit = {
      val innerWriter = ObjectFormatUtils.writeBeginObject(writer)

      for (k <- value.keysIterator) {
        innerWriter.key(kf.encode(k))
        vf.encode(innerWriter, value.items(k))
      }

      ObjectFormatUtils.writeEndObject(writer)
    }

    def decode(reader: JsonReader): OrderedMap[K, V] = {
      val items = Map.newBuilder[K, V]
      val order = Vector.newBuilder[K]
      val innerReader = ObjectFormatUtils.expectBeginObject(reader)

      while (innerReader.peekToken == JsonToken.TKey) {
        val k = kf.decode(innerReader.key.toString)
        innerReader.nextToken()

        val v = vf.decode(innerReader)

        items += k -> v
        order += k
      }

      ObjectFormatUtils.expectEndObject(reader)
      new OrderedMap[K, V](items.result(), order.result())
    }
  }

  given mapConversion[K, V](using Ordering[K]): Conversion[Map[K, V], OrderedMap[K, V]] with {
    def apply(x: Map[K, V]): OrderedMap[K, V] = {
      val order = x.keys.toVector.sorted
      new OrderedMap[K, V](x, order)
    }
  }

  /** Create new ordered map from pairs */
  def apply[K, V](pairs: (K, V)*): OrderedMap[K, V] =
    new OrderedMap[K, V](Map(pairs:_*), pairs.map(_._1).toVector)
}

/** Helper to retain ordering of map elements */
class OrderedMap[K, V](val items: Map[K, V], val order: Seq[K]) {
  assert(items.size == order.size && items.keySet == order.toSet)

  def apply(key: K): V = items(key)
  def get(key: K): Option[V] = items.get(key)
  def contains(key: K): Boolean = items.contains(key)
  def keysIterator: Iterator[K] = order.iterator
}
