package s3j.macros

import s3j.macros.generic.{GenerationConfidence, GenerationMode}
import s3j.macros.modifiers.ModifierSet
import s3j.macros.traits.{BasicPluginContext, ErrorReporting, NestedBuilder, ReportingBuilder}
import s3j.macros.utils.GenerationPath

import scala.quoted.{Expr, Quotes, Type}

object GenerationContext {
  sealed trait GenerationOutcome

  /**
   * Outcome for situations where plugin is completely unable to deal with request. If no candidates will be found,
   * error message will not mention this specific plugin (implying that it's obvious for user why plugin wasn't chosen).
   */
  case object GenerationUnsupported extends GenerationOutcome

  /**
   * Outcome for situations where plugin is generally capable to handle the request, but some reasons prevented it from
   * handling _this_ specific request, implying that these reason may not be obvious for user. If no candidates will be
   * found, a message will be displayed to explain why plugin wasn't chosen.
   */
  case class GenerationRejection(message: String) extends GenerationOutcome

  /**
   * Positive generation outcome. All candidates are sorted by their confidence, and most confident candidate is
   * selected. If confidence is specified as `None`, candidate is always selected. Having two such candidates at same
   * time will result in compilation error. `None` is meant for situations where plugin is completely sure that it
   * should handle the request - e.g. when user explicitly called it with some annotation. On other hand, confidence is
   * meant for 'generic' plugins that handle broad classes of objects - the more specific plugin is, the higher
   * confidence is assigned to the result.
   */
  trait GenerationCandidate extends GenerationOutcome {
    /** @return Confidence level of this candidate */
    def confidence: GenerationConfidence

    /**
     * Generate serializer identity for deduplication. Note that identity should include only configuration data,
     * and should not include any identities of nested types. This implies that any modifiers present in the set SHOULD
     * NOT change
     *
     * @return Generated serializer identity for deduplication. 3-tuple of (type, plugin, identity) is a key for
     *         deduplication. Identity is only queried after candidate is selected, so it's possible to do expensive
     *         computations here.
     */
    def identity: AnyRef

    /** @return Actual generated code for serializer */
    def generate(using Quotes)(): Expr[Any]

    /** @return Whether generated code is something simple, and recursive wrapper could be avoided for it */
    def simpleGeneration: Boolean = false
  }
}

trait GenerationContext extends BasicPluginContext {
  /** @return New builder for nested serializer */
  def nested[T](using Type[T]): NestedBuilder[T]

  /** @return New builder for nested serializer */
  def nested(using q: Quotes)(t: q.reflect.TypeRepr): NestedBuilder[?]

  /** @return New report builder to create derived ErrorReporting instances */
  def reportBuilder: ReportingBuilder
}
