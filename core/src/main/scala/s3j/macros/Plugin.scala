package s3j.macros

import s3j.annotations.usePlugin
import s3j.macros.GenerationContext.{GenerationOutcome, GenerationUnsupported, GenerationRejection, GenerationCandidate}
import s3j.macros.generic.Extensions
import s3j.macros.modifiers.{ModifierParser, ModifierSet}

import scala.quoted.{Quotes, Type}

/**
 * Entry point to extending s3j's generation process with your code. Use [[usePlugin]] annotation to have it loaded in
 * runtime.
 */
abstract class Plugin {
  /** @return Human-readable plugin name */
  def name: String

  /**
   * Plugins could extend functionality of other plugins by registering a target-specific extensions.
   *
   * @return Extensions registered by this plugin
   */
  def extensions: Extensions = Extensions.empty
  
  /** @return Plugin capabilities */
  def capabilities: Set[PluginCapability] = Set.empty
  
  /** @return Extra locations to look for implicits (module symbols) */
  def implicitLocations(using q: Quotes): Set[q.reflect.Symbol] = Set.empty
  
  /** @return Modifier parser for this plugin */
  def modifierParser(using PluginContext): ModifierParser = ModifierParser.empty

  /**
   * Perform generation for a type with specified modifiers. Plugin should decide whether it is able to perform that
   * generation, and return one of following generation outcomes:
   *
   *  1. [[GenerationUnsupported]] when plugin is completely unable to handle that type, or modifiers are inappropriate
   *
   *  1. [[GenerationRejection]] when plugin is able to handle that type, but some reasons prevented it from handling
   *     this specific request. Error message will be displayed to user if no other plugin will be chosen.
   *
   *  1. [[GenerationCandidate]] when plugin is ready to generate the serializer.
   *
   * @tparam T        Type for requested serializer. Use given `Type[T]` to inspect it further.
   * @param modifiers Effective modifier set for generation
   * @param Quotes    [[Quotes]] for type inspection (separate [[Quotes]] will be provided for actual generation)
   * @return          Generation candidate
   */
  def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome =
    GenerationUnsupported
}
