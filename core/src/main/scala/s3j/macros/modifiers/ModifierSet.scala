package s3j.macros.modifiers

import scala.annotation.targetName

object ModifierSet {
  /** Empty modifier set */
  val empty: ModifierSet = new ModifierSet(Map.empty)

  /** Create new modifier set containing specified modifiers */
  def apply(xs: Modifier*): ModifierSet = new ModifierSet(xs.map(x => x.key -> x).toMap)

  /** Compute inherited modifier set */
  def inherit(parent: ModifierSet, own: ModifierSet): ModifierSet =
    new ModifierSet(parent.items.filter(_._2.inheritable) ++ own.items)

  /**
   * Compute inherited modifier set from multiple parents. First element is a most distant parent, last element is an
   * own modifier set for the target
   */
  def inherit(xs: ModifierSet*): ModifierSet =
    xs.reduceLeft(ModifierSet.inherit)
}

/** Parsed modifier set */
final class ModifierSet private(private val items: Map[ModifierKey[_], Modifier]) {
  /** @return true when this set contains a modifier with given key */
  def contains(key: ModifierKey[?]): Boolean = items.contains(key)

  /** @return true when this set contains any of the keys */
  def containsAny(keys: ModifierKey[?]*): Boolean = keys.exists(contains)
  
  /** @return Option with associated modifier, or key's default value */
  def get[T <: Modifier](key: ModifierKey[T]): Option[T] = items.get(key).orElse(key.default).asInstanceOf[Option[T]]

  /** @return Associated modifier (throws when no modifier is present) */
  def apply[T <: Modifier](key: ModifierKey[T]): T =
    get(key).getOrElse(throw new NoSuchElementException(s"Modifier set has no entry for '$key'"))

  /** @return Set of present modifier keys */
  def keys: Set[ModifierKey[_]] = items.keySet

  /** @return Set of present modifiers */
  def values: Iterable[Modifier] = items.values

  /** @return Modifier set with added modifier, overwriting any previously present modifier with that key */
  @targetName("add")
  def +(x: Modifier): ModifierSet = new ModifierSet(items + (x.key -> x))

  /** @return Modifier set with added modifiers (newly added modifiers take precedence over already existing) */
  @targetName("addAll")
  def ++(xs: Iterable[Modifier]): ModifierSet = new ModifierSet(items ++ xs.map(x => x.key -> x))

  /** @return Modifier set with added modifiers (newly added modifiers take precedence over already existing) */
  @targetName("addAll")
  def ++(xs: ModifierSet): ModifierSet = new ModifierSet(items ++ xs.items)

  /** @return Modifier set with given modifier removed */
  @targetName("remove")
  def -(key: ModifierKey[_]): ModifierSet = new ModifierSet(items - key)

  /** @return Modifier set with given modifiers removed */
  @targetName("removeAll")
  def --(keys: Iterable[ModifierKey[_]]): ModifierSet = new ModifierSet(items -- keys)

  override def toString: String = s"[${items.values.mkString(", ")}]"
}
