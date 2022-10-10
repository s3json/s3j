package s3j.format

import s3j.format.impl.{IterableFormat, MapFormat}

import scala.collection.Factory

object CollectionFormats {
  given mapEncoder[K, V](using k: StringyEncoder[K], v: JsonEncoder[V]): JsonEncoder[Map[K, V]] =
    new MapFormat[K, V](k, null, v, null)

  given mapDecoder[K, V](using k: StringyDecoder[K], v: JsonDecoder[V]): JsonDecoder[Map[K, V]] =
    new MapFormat[K, V](null, k, null, v)

  given mapFormat[K, V](using k: StringyFormat[K], v: JsonFormat[V]): JsonFormat[Map[K, V]] =
    new MapFormat[K, V](k, k, v, v)

  given iterableEncoder[T, C <: Iterable[T]](using enc: JsonEncoder[T]): JsonEncoder[C] =
    new IterableFormat[T, C](enc, null, null)

  given iterableDecoder[T, C <: Iterable[T]](using dec: JsonDecoder[T], f: Factory[T, C]): JsonDecoder[C] =
    new IterableFormat[T, C](null, dec, f)

  given iterableFormat[T, C <: Iterable[T]](using fmt: JsonFormat[T], f: Factory[T, C]): JsonFormat[C] =
    new IterableFormat[T, C](fmt, fmt, f)
}
