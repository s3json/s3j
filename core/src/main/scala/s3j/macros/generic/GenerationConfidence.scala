package s3j.macros.generic

object GenerationConfidence {
  given intConfidenceConversion: Conversion[Int, GenerationConfidence] = GenerationConfidence.Uncertain(_)
}

enum GenerationConfidence {
  /** Plugin is certain that candidate is the right one. Two such candidates cause conflict. */
  case Certain

  /** Plugin is uncertain about that candidate. Candidate with highest certainty is selected. */
  case Uncertain(certainty: Int)
  
  /** @return Whether this confidence is [[Certain]] */
  def isCertain: Boolean = this == Certain
  
  /** @return Whether this confidence is not a [[Certain]] */
  def nonCertain: Boolean = this != Certain
  
  /** @return Numeric value for confidence */
  def value: Int = this match {
    case Certain => Int.MaxValue
    case Uncertain(v) => v
  }
}
