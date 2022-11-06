package s3j.macros

import s3j.macros.FreshPluginContext.StackHandle
import s3j.macros.generic.GenerationMode
import s3j.macros.modifiers.ModifierSet
import s3j.macros.traits.GenerationResult

import scala.quoted.{Quotes, Expr, Type}

object FreshPluginContext {
  trait StackHandle[T] {
    given staticType: Type[T]

    /**
     * Set a definition for the stack entry. This method could be called only once.
     *
     * @param value Definition for this entry
     * @tparam U    Actual type of definition, which could be more specific than overall type bound of this entry.
     */
    def setDefinition(value: Expr[T]): Unit
    
    /** @return Quotes instance for generation of definition */
    def nestedQuotes: Quotes
    
    /**
     * @return Reference to this entry to access it after generation
     */
    def reference: Expr[T]
  }
}

trait FreshPluginContext extends PluginContext {
  /**
   * Add a customized entry to current codec stack.
   *
   * @param name Name of the entry
   * @tparam T   Type of contained value
   * @return     Hande to interact with added entry
   */
  def addStackEntry[T](name: String)(using Type[T]): StackHandle[T]

  /**
   * Generate codec for given type, adding it to current codec stack.
   *
   * @param mode Generation mode
   * @param modifiers Modifier set to use
   * @tparam T   Inner type of generated codec
   * @return     Codec handle
   */
  def generate[T](mode: GenerationMode, modifiers: ModifierSet)(using Type[T]): GenerationResult[T]

  /**
   * Finalize codec stack, producing complete generation result
   * 
   * @param result Returned value
   * @return       Codec stack with returned value
   */
  def result[T](result: Expr[T])(using Type[T]): Expr[T]
}
