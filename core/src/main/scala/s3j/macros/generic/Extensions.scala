package s3j.macros.generic

import s3j.macros.Plugin

import scala.annotation.targetName
import scala.collection.mutable

object Extensions {
  /** Type-safe pair for heterogeneous keys */
  case class TypedPair[T](key: Key[T], value: T) {
    override def toString: String = s"$key ~> $value"
  }

  /** Typed extension key. Always has an identity equality semantics, name serves only for debugging */
  class Key[T](val name: String) {
    /** @return a typed pair for insertion into extension map */
    @targetName("makePair")
    def ~>(value: T): TypedPair[T] = TypedPair(this, value)

    override final def hashCode(): Int = super.hashCode()
    override final def equals(x: Any): Boolean = super.equals(x)
    override final def toString: String = name
  }

  val empty: Extensions = new Extensions(Map.empty)

  /** Create new extensions map from supplied pairs */
  def apply(xs: TypedPair[_]*): Extensions =
    new Extensions(xs.groupMap(_.key)(_.value).map { case (k, vs) => k -> vs.toSet })
  
  /** @return Extension key */
  def key[T](name: String): Key[T] = new Extensions.Key[T](name)
}

/**
 * Heterogeneous multimap to hold plugin's available extensions. This allows plugin extensions to be found much more
 * efficiently than trying to cast every possible plugin to every possible trait.
 */
class Extensions private(val untypedMap: Map[Extensions.Key[_], Set[Any]]) {
  override def toString: String = s"Extensions(${untypedMap.mkString(", ")})"
}
