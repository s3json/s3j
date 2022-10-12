package s3j.macros.generic

import scala.quoted.{Quotes, quotes}

object ImplicitBehavior {
  /** Neutral implicit behavior that does not affect implicit search */
  def Neutral: ImplicitBehavior = NeutralBehavior

  /** @return Behavior which suppresses implicit search */
  def Suppress: ImplicitBehavior = SuppressionBehavior

  /** @return Behavior which extends implicit search locations */
  def Extend(using Quotes)(locations: quotes.reflect.Symbol*): ImplicitBehavior =
    new ExtensionBehavior(locations.toSet.asInstanceOf[Set[AnyRef]])
}

sealed trait ImplicitBehavior {
  /** @return Whether implicit search is completely supressed */
  def suppressed: Boolean

  /** @return Extra search locations to import */
  def extraLocations(using Quotes): Set[quotes.reflect.Symbol]
}

private case object NeutralBehavior extends ImplicitBehavior {
  def suppressed: Boolean = false
  def extraLocations(using Quotes): Set[quotes.reflect.Symbol] = Set.empty
  override def toString: String = s"Neutral"
}

private case object SuppressionBehavior extends ImplicitBehavior {
  def suppressed: Boolean = true
  def extraLocations(using Quotes): Set[quotes.reflect.Symbol] = Set.empty
  override def toString: String = s"Suppression"
}

private class ExtensionBehavior(locations: Set[AnyRef]) extends ImplicitBehavior {
  def suppressed: Boolean = false
  def suppressionReason: Option[String] = None
  def extraLocations(using Quotes): Set[quotes.reflect.Symbol] = locations.asInstanceOf[Set[quotes.reflect.Symbol]]
  override def toString: String = s"Extend(${locations.mkString(", ")})"
}
