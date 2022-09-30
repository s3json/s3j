package s3j.macros.utils

/**
 * Wrapping dropping all overridden equals() and hashCode() implementations, and reverting the semantics of these
 * operations to identity-based one. Useful to converting any `Map` or `Set` to `IdentityMap` or `IdentitySet`.
 */
final case class IdentityWrapper[T <: AnyRef](inner: T) {
  override def hashCode: Int = System.identityHashCode(inner)
  
  override def equals(obj: Any): Boolean = obj match {
    case obj: AnyRef => inner eq obj
    case _ => false
  }

  override def canEqual(that: Any): Boolean = equals(that)
}
